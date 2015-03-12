{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-

This code is placed in the Public Domain.

Library code for training a Markov chain on data and then using it to
create gibberish.

At present it uses an order 2 chain, using "words" as the token type. It
is designed for English text; for languages with not too disimilar
character classes it should be okay, but it is not going to support
all languages. It has very-limited discrimination of tokens, basically
only end-of-paragraph.

There is no support for versioning data; that is, the file saved by
writeMarkov is not guaranteed to be read in by a later version of
readMarkov. This could be done - e.g. with SafeCopy - but I have no
desire to do so at this time.

This module defines orphan instances.

TODO:

  - should there be a separate mode for creating titles versus more
    long-form text?

  - it would be nice if the chain could be cut off at the end of a
    sentence (for those inputs that have sentence-like structure).

-}

module Chain
       ( Token
       , toToken
       , fromToken
       , lenToken
       , tokenize

       , Key
       , TransitionWeight
       , TransitionWeights
       , Markov
       , buildMarkov

       , runMarkov
       , seedMarkov

       , writeMarkov
       , readMarkov
       , infoMarkov
       , compareMarkov

       , getSeed
       ) where

import qualified Control.Monad.Random as R

import qualified Data.ByteString.Lazy as LB

-- I have done no profiling to see whether it's worth skipping
-- the default Map and jumping to unordered-containers. I just
-- did it.
--
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import Control.Arrow (second)

import Data.Char (isUpper)
import Data.Hashable (Hashable(..))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Ratio (numerator, denominator)
import Data.Serialize
import Data.Time (UTCTime(..), getCurrentTime)

import GHC.Generics (Generic)

import System.CPUTime (getCPUTime)
import System.Random (mkStdGen)

-- | I could use a boolean here, but as it's so easy to provide a
--   "semantically meaningful" type, let's do that.
--
data TokenType = GeneralToken | EndParaToken
  deriving (Eq, Ord, Generic)

-- | Tokens are considered to be \"white-space separated words\", and
--   can include punctuation. However, the only restriction is that
--   they can not be empty.
--
--   Comparison is "exact", in the sense that \"foo\" and \"Foo\"
--   are different.
--
data Token = Token { tkContents :: T.Text
                   , tkType     :: TokenType -- is this worth it?
                   }
  deriving (Eq, Ord, Generic)

-- Should the token contents contain the trailing \n for EndParaToken types?

toToken :: TokenType -> T.Text -> Maybe Token
toToken ty t | T.null t  = Nothing
             | otherwise = Just Token { tkContents = t, tkType = ty }

gToken, eToken :: T.Text -> Maybe Token
gToken = toToken GeneralToken
eToken = toToken EndParaToken

fromToken :: Token -> T.Text
fromToken t =
  let txt = tkContents t
  in case tkType t of
       GeneralToken -> txt
       EndParaToken -> txt `T.snoc` '\n'

lenToken :: Token -> Int
lenToken t = 
  let len = T.length (tkContents t)
  in case tkType t of
       GeneralToken -> len 
       EndParaToken -> 1 + len

-- Would the following be inlined (ditto for hashWithSalt)? I guess so
--     hash (Token t p) = hash (t,p)
--
instance Hashable Token where
  hashWithSalt s (Token t p) = s `hashWithSalt` p `hashWithSalt` t
  hash (Token t p) = hash p `hashWithSalt` t

instance Hashable TokenType where
  hashWithSalt s GeneralToken = hashWithSalt s (0::Int)
  hashWithSalt s EndParaToken = hashWithSalt s (1::Int)

-- Hand roll a simple Markov generator. I use a slightly
-- different type for running the chain than for building
-- it; as the code is currently written this seems an
-- unnescessary optimisation.
--
type Key = (Token, Token)

type TransitionMap = M.HashMap Token Int
type MarkovBuild = M.HashMap Key TransitionMap

type TransitionWeight = (Token, Rational)
type TransitionWeights = [TransitionWeight]

-- | The \"trained\" Markov chain.
--
data Markov = Markov
  { mvMap :: M.HashMap Key TransitionWeights
  , mvStart :: [Key] -- should this use a NonEmptyList representation?
  } deriving Generic

-- Need to convert between text and bytestring
--
instance Serialize T.Text where
  put = put . TE.encodeUtf8 
  get = fmap TE.decodeUtf8 get

instance (Serialize k, Hashable k, Eq k, Serialize v) 
          => Serialize (M.HashMap k v) where
  put = put . M.toList 
  get = fmap M.fromList get

instance Serialize TokenType
instance Serialize Token

-- Is it worth only serializing the map - e.g. re-create the mvStart
-- array on read in?
instance Serialize Markov

-- | This is a very-simple scheme:
--
--    - use words to separate tokens, which means that trailing or
--      leading punctuation - such as .,"'() - will be included.
--
--    - a blank line separates paragraphs, in which case the "\n"
--      is included in the last token of the paragraph, as a simple
--      way to simulate paragraphs.
--
--    - the last token is forced to mark 'end paragraph'; perhaps it
--      should be 'end text'?
--
tokenize :: T.Text -> [Token]
tokenize txt = concatMap tokenizePara (makeParas txt)

-- | Split up a string into a list of paragraphs, indicated by having
--   one or more blank lines separating text.
--
--   The grouping works with reversed lists.
--
type Para = [T.Text]

groupParas :: (Para,[Para]) -> T.Text -> (Para,[Para])
groupParas (c,ps) l = if T.null l
                      then ([], if null c then ps else c:ps)
                      else (if null c then [l] else l:c, ps)

-- This reverses the order of the paragraphs, but not the paragraph
-- contents. As the paragraphs are treated as separate chunks this
-- should not be a problem.
makeParas :: T.Text -> [Para]
makeParas txt = 
  let (pl,ps) = foldl' groupParas ([],[]) (map T.strip (T.lines txt))
  in map reverse (pl:ps)

-- Probably not very efficient.
tokenizePara :: Para -> [Token]
tokenizePara ps = 
  let ws = concatMap T.words ps
  in case reverse ws of
    [] -> []
    (x:xs) -> reverse $ catMaybes $ eToken x : map gToken xs

addTriple :: MarkovBuild -> (Token,Token,Token) -> MarkovBuild
addTriple m (f,s,w) = 
  let v0 = M.singleton w 1

      -- If updateT is called then the first argument is v0,
      -- so that we can just ignore this argument.
      --
      updateT :: TransitionMap -> TransitionMap -> TransitionMap
      updateT _ = M.insertWith (+) w 1

  in M.insertWith updateT (f,s) v0 m

-- Add additional tokens to the training data.
addTokens :: MarkovBuild -> [Token] -> MarkovBuild
addTokens orig toks =
  let toks1 = tail toks
      toks2 = tail toks1
      triples = zip3 toks toks1 toks2
  in foldl' addTriple orig triples
 
-- Train on a list of token-lists.
initialize :: [[Token]] -> MarkovBuild
initialize = foldl' addTokens M.empty

-- | Converts the build form of the chain into the version
--   used to create an instance of a chain.
--
--   It returns @Nothing@ if there are no
--   keys - i.e. possible starting word pairs - 
--   for which the first word is capitalized.
--
convert :: MarkovBuild -> Maybe Markov
convert m = 
  let wanted t = case T.uncons (tkContents t) of
                   Nothing -> error "Token invariant invalidated!"
                   Just (c,_) -> isUpper c

      start = filter (wanted.fst) $ M.keys m
      out = Markov { mvMap = M.map (map (second toRational). M.toList) m
                   , mvStart = start
                   }

  in if null start then Nothing else Just out

-- | Train a chain on the input tokens. The return is @Nothing@
--   if the final set has no capitalized words to start the chain.
--
buildMarkov :: [[Token]] -> Maybe Markov
buildMarkov = convert . initialize 

-- | Pick a start pair from the set, ensuring that the
--   first token is capitalized.
--
--   This *assumes* that there is a capitalized starting
--   token in the training set.
--
startToken :: R.MonadRandom m => Markov -> m Key
startToken m = R.uniform $ mvStart m

-- Carry around the length of the builder   
type Builder = (Int, TB.Builder)

-- | This is only intended for short output sequences.
--
buildChain :: 
  R.MonadRandom m 
  => Int    -- ^ Maximum length, in characters.
  -> Markov 
  -> Builder
  -> Key 
  -> m Builder
buildChain maxlen m orig start@(_,stok) = 
  case M.lookup start (mvMap m) of
    Nothing -> return orig
    Just transitions -> do
      ntok <- R.fromList transitions
      let next = (stok, ntok)
          newbld = addToken orig next

      if fst newbld > maxlen
        then return orig
        else buildChain maxlen m newbld next

-- Work out how to combine the next token with the current builder.
--
addToken :: Builder -> Key -> Builder
addToken (l1,b1) (t1,t2) = 
  let txt = fromToken t2
      l2  = T.length txt
      b2  = TB.fromText txt

      lout = l1 + 1 + l2
      istr = case tkType t1 of
               EndParaToken -> "\n" -- note: b1 will end with \n so only need 1
               GeneralToken -> " " 

  in (lout, b1 <> istr <> b2)

-- Stop the chain if the word takes the total length past the
-- input len. As a simplification, the check is not applied
-- to the starting pair.
--
runMarkov :: R.MonadRandom m => Int -> Markov -> m T.Text
runMarkov maxlen markov = do
  start@(s1,s2) <- startToken markov
  -- by construction s1 should not be an EndParaToken
  let t1 = fromToken s1
      t2 = fromToken s2
      bld = TB.fromText t1 <> " " <> TB.fromText t2
      l = 1 + T.length t1 + T.length t2
  out <- buildChain maxlen markov (l,bld) start
  return $ LT.toStrict $ TB.toLazyText $ snd out

-- | A version of runMarkov where the seed is explicit.
--
seedMarkov :: 
  Int  -- ^ Maximum number of characters
  -> Markov 
  -> Int  -- ^ Seed for random-number generator
  -> IO T.Text
seedMarkov maxlen markov seed = R.evalRandT chain gen
  where
    chain = runMarkov maxlen markov
    gen = mkStdGen seed

-- For now use a binary serialization of the structure. I could,
-- and perhaps should, use JSON, but I want to use the cereal library.
--

-- | Write out the chain as a binary file, overwriting any existing file.
-- 
writeMarkov :: Markov -> FilePath -> IO ()
writeMarkov m fname = LB.writeFile fname $ encodeLazy m

-- | Read in the chain from the binary file.
--
--   Note that there is *no* attempt at upgrading data files when
--   types change, or even version checking. So, expect to see
--   error messages like
--
--      Failed reading: Unknown encoding for constructor
--
--   when reading in old/invalid files.
--
readMarkov :: FilePath -> IO (Either String Markov)
readMarkov fname = decodeLazy `fmap` LB.readFile fname

-- | Based on System.Random.mkStdRNG, but just returns an `Int` value
--   that can be used to seed a new generator.
--
getSeed :: IO Int
getSeed = do
  cTime <- getCPUTime
  utc <- getCurrentTime
  let daytime = toRational $ utctDayTime utc
      (sec, psec) = numerator daytime `quotRem` denominator daytime
  return $ fromIntegral $ sec * 12345 + psec + cTime

-- | Return some basic information on a Markov chain.
--
--   The return is a list of "key,value" pairs (since I can not be
--   bothered to code up a structure when I do not know what information
--   I want to return here).
--
infoMarkov :: Markov -> [(String, Int)]
infoMarkov mv = 
  let m = mvMap mv
  in [ ("Number of keys", M.size m)
     , ("Number of start keys", length (mvStart mv))
     ]

-- | Report some basic information on the overlap between the two chains.
--
--   The return is a list of "key,value" pairs (since I can not be
--   bothered to code up a structure when I do not know what information
--   I want to return here).
--
compareMarkov :: Markov -> Markov -> [(String, Int)]
compareMarkov mv1 mv2 =
  let m1 = mvMap mv1
      m2 = mvMap mv2
      k1 = S.fromList (M.keys m1)
      k2 = S.fromList (M.keys m2)
      both = k1 `S.intersection` k2
  in [ ("Number of keys (chain 1)", M.size m1)
     , ("Number of keys (chain 2)", M.size m2)
     , ("Number of keys in both", S.size both)
     ]

