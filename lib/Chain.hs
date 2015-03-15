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
only end-of-paragraph/end-of-text.

There is no support for versioning data; that is, the file saved by
writeMarkov is not guaranteed to be read in by a later version of
readMarkov. This could be done - e.g. with SafeCopy - but I have no
desire to do so at this time.

This module defines orphan instances.

TODO:

  - Should there be a separate mode for creating titles versus more
    long-form text?

  - It would be nice if the chain could be cut off at the end of a
    sentence (for those inputs that have sentence-like structure).

-}

module Chain
       ( Token
       , toToken
       , fromToken
       , dumpToken
       , lenToken
       , tokenize

       , Key
       , TransitionFraction
       , fromFrac
       , TransitionWeight
       , TransitionWeights
       , Markov
       , buildMarkov
       , combineMarkov

       , runMarkov
       , seedMarkov

       , writeMarkov
       , readMarkov
       , infoMarkov
       , compareMarkov
       , getTransitions

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

import qualified Data.List.NonEmpty as NE

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import Control.Arrow (second)

import Data.Char (isUpper)
import Data.Hashable (Hashable(..))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Ratio (numerator, denominator)
import Data.Semigroup
import Data.Serialize
import Data.Time (UTCTime(..), getCurrentTime)

import GHC.Generics (Generic)

import System.CPUTime (getCPUTime)
import System.Random (mkStdGen)

-- | Tokens are considered to be \"white-space separated words\", and
--   can include punctuation. However, the only restriction is that
--   they can not be empty.
--
--   Comparison is "exact", in the sense that \"foo\" and \"Foo\"
--   are different.
--
data Token = TextToken T.Text | EndParaToken | EndToken
  deriving (Eq, Ord, Generic)

toToken :: T.Text -> Maybe Token
toToken t | T.null t  = Nothing
          | otherwise = Just (TextToken t)

fromToken :: Token -> T.Text
fromToken EndToken      = ""
fromToken EndParaToken  = "\n\n"
fromToken (TextToken t) = t

dumpToken :: Token -> T.Text
dumpToken EndToken      = "\\n"
dumpToken EndParaToken  = "\\n\\n"
dumpToken (TextToken t) = t

lenToken :: Token -> Int
lenToken EndToken      = 0
lenToken EndParaToken  = 2
lenToken (TextToken t) = T.length t

-- Since tokens can not be empty, this should be okay.
instance Hashable Token where
  hashWithSalt s EndToken      = hashWithSalt s T.empty
  hashWithSalt s EndParaToken  = hashWithSalt s (T.pack "\n\n")
  hashWithSalt s (TextToken t) = hashWithSalt s t

-- Hand roll a simple Markov generator. I use a slightly
-- different type for running the chain than for building
-- it; as the code is currently written this seems an
-- unnescessary optimisation.
--
-- It would be nice to recognize that a key with the
-- first element being EndToken is invalid.
type Key = (Token, Token)

type TransitionMap = M.HashMap Token Int
type MarkovBuild = M.HashMap Key TransitionMap

-- Could use a non-empty list here for TransitionWeights? There's
-- a cost to be paid whenever a token is randomly extracted, since
-- there has to be a conversion from NonEmpty a to [a], but of the
-- output text is meant to be small, then this is unlikely to be
-- remotely important.
--

-- type TransitionWeight = (Token, Rational)
type TransitionWeight = (Token, TransitionFraction)

-- I use a newtype so that I can serialize it as an int to see if
-- it saves space. It does, but it's not a huge amount (e.g.
-- several MB for a 30 MB file). I have tried to make sure that
-- the constaint that the rational instance has a denominator of 1
-- is handled by the type system, but it is an implicit constraint;
-- i.e. new code could break it without a type error.
--
newtype TransitionFraction = TF Rational

-- Note: the conversions are asymmetric to ensure you can not make
-- a fraction (i.e. denominator is not 1).
--
toFrac :: Int -> TransitionFraction
toFrac = TF . toRational

fromFrac :: TransitionFraction -> Rational
fromFrac (TF x) = x

-- I originally had derived a Num instance for TransitionFraction
-- but that provides more functionality than we need (particularly
-- as I want to keep the constraint that the Rational values
-- stored within TF all have a denominator of 1).
--
addFrac :: TransitionFraction -> TransitionFraction -> TransitionFraction
addFrac (TF x) (TF y) = TF (x+y)

type TransitionWeights = NE.NonEmpty TransitionWeight
-- type TransitionWeights = [TransitionWeight]

-- | The \"trained\" Markov chain.
--
data Markov = Markov
  { mvMap :: M.HashMap Key TransitionWeights
  , mvStart :: NE.NonEmpty Key
  } deriving Generic

-- | What are the transitions in the chain?
getTransitions :: Markov -> M.HashMap Key TransitionWeights
getTransitions = mvMap

instance Semigroup Markov where
  (<>) = combineMarkov

-- Need to convert between text and bytestring
--
instance Serialize T.Text where
  put = put . TE.encodeUtf8 
  get = fmap TE.decodeUtf8 get

instance (Serialize k, Hashable k, Eq k, Serialize v) 
          => Serialize (M.HashMap k v) where
  put = put . M.toList 
  get = fmap M.fromList get

instance Serialize a => Serialize (NE.NonEmpty a) where
  put = put . NE.toList
  get = fmap NE.fromList get

instance Serialize Token

-- Assume that the fraction always has a denominator of 1 and
-- that we can convert from an Integer to Int without worrying
-- about overflow. I have attempted to provide an interface
-- to TransitionFraction that enforces this constraint, but
-- there's no guarantee (particularly here, where the internals
-- are exposed).
--
instance Serialize TransitionFraction where
  put = let conv :: TransitionFraction -> Int
            conv = fromIntegral . numerator . fromFrac
        in put . conv
  get = fmap toFrac get

-- Is it worth only serializing the map - e.g. re-create the mvStart
-- array on read in?
instance Serialize Markov

-- | This is a very-simple scheme:
--
--    - use words to separate tokens, which means that trailing or
--      leading punctuation - such as .,"'() - will be included.
--
--    - a blank line separates paragraphs
--
tokenize :: T.Text -> [Token]
tokenize txt =
  let t1 = tokenizePara EndParaToken
      t2 = tokenizePara EndToken 
  in case makeParas txt of
    [] -> []
    (x:xs) -> concat (reverse (t2 x : map t1 xs))

-- | Split up a string into a list of paragraphs, indicated by having
--   one or more blank lines separating text.
--
--   The grouping works with reversed lists. For now I just use type
--   synonymns to avoid constructing/deconstructing types, but
--   really should use a newtype to provide type safety.
--
type Para = [T.Text]

-- RevPara
--   lines of the paragraph are reversed but the lines themselves are correct
-- RevParas
--   paragraphs are in reverse order but the paras are correct
-- R2Paras
--   paragraphs are in reverse order, and lines of paragraphs are reversed
--   
type RevPara = Para
type RevParas = [Para]
type R2Paras = [RevPara]

-- A list of paragraphs that have been placed in reversed order,
-- such as para3, para2, para1, but where the contents of each
-- paragraph is ordered correctly.
--
groupParas :: (RevPara,R2Paras) -> T.Text -> (RevPara,R2Paras)
groupParas (c,ps) l
  | T.null l  = ([], if null c then ps else c:ps)
  | otherwise = (if null c then [l] else l:c, ps)

-- | It's important that this returns a revsed list of paragraphs,
--   to make tokenize easier.
makeParas :: T.Text -> RevParas
makeParas txt = 
  let stxt = T.strip txt
      (pl,ps) = foldl' groupParas ([],[]) (map T.strip (T.lines stxt))
  in map reverse (pl:ps)

-- Probably not very efficient.
--
tokenizePara :: Token -> Para -> [Token]
tokenizePara lastToken ps = 
  let ws = concatMap T.words ps
  in case reverse ws of
    [] -> []
    xs -> reverse $ catMaybes $ Just lastToken : map toToken xs

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

-- | Converts the build form of the chain into the version used to
-- create an instance of a chain.
--
--   It returns @Nothing@ if there are no keys - i.e. possible
--   starting word pairs - for which the first word is
--   capitalized. The restrictions on what is needed to be
--   a \""starting pair\" includes:
--
--      - first token is capitalized
--      - both tokens are text
--
convert :: MarkovBuild -> Maybe Markov
convert m = do
  let wanted (TextToken t1, TextToken _) =
        case T.uncons t1 of
          Nothing -> error "Token invariant invalidated!"
          Just (c,_) -> isUpper c
      wanted (_,_) = False

      start = filter wanted (M.keys m)

      -- It should be safe to use NE.fromList here, due to the
      -- way the MarkovBuild structure is built.
      toFreq :: TransitionMap -> TransitionWeights
      toFreq = NE.fromList . map (second toFrac) . M.toList

      mmap  = M.map toFreq m

  keys <- NE.nonEmpty start
  return Markov { mvMap = mmap
                , mvStart = keys
                }

-- | Train a chain on the input tokens. The return is @Nothing@
--   if the final set has no capitalized words to start the chain.
--
buildMarkov :: [[Token]] -> Maybe Markov
buildMarkov = convert . initialize 

-- | Pick a start pair from the set, ensuring that the
--   first token is capitalized.
--
startToken :: R.MonadRandom m => Markov -> m Key
startToken = R.uniform . NE.toList . mvStart 
-- having to convert to a list may not be efficient, but let's worry about
-- that if I ever get around to profiling the code

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
      -- is the unwrapping of the TransitionFraction compiled away?
      -- ntok <- R.fromList (NE.toList transitions)
      ntok <- R.fromList (map (second fromFrac) (NE.toList transitions))
      let next = (stok, ntok)
          newbld = addToken orig next

      if fst newbld > maxlen
        then return orig
        else buildChain maxlen m newbld next

-- Work out how to combine the next token with the current builder.
-- 
addToken :: Builder -> Key -> Builder
addToken orig (_,EndToken) = orig
addToken orig (EndParaToken,EndParaToken) = orig
addToken (l1,b1) (_,EndParaToken) = (l1 + 2, b1 <> "\n\n")
addToken (l1,b1) (TextToken _,TextToken t2) = (l1 + l2, b1 <> " " <> b2)
  where
    l2 = 1 + T.length t2
    b2 = TB.fromText t2
addToken (l1,b1) (_,TextToken t2) = (l1 + l2, b1 <> b2)
  where
    l2 = T.length t2
    b2 = TB.fromText t2
  
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
-- and perhaps should, use JSON, but I want to try out the cereal library.
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

-- | Combine the two chains.
--
--   It is probable that the first chain should be the "smaller",
--   for efficient combination, but I haven't actually benchmarked
--   it.
--
combineMarkov :: Markov -> Markov -> Markov
combineMarkov (Markov m1 s1) (Markov m2 s2) = 
  let conv = S.fromList . NE.toList
      -- NE.fromList will error out if the input is empty, but this
      -- is not possible here.
      s12 = NE.fromList (S.toList (conv s1 `S.union` conv s2))
      m12 = M.unionWith combineWeights m1 m2
  in Markov m12 s12

combineWeights :: TransitionWeights -> TransitionWeights -> TransitionWeights
combineWeights w1 w2 = 
  let m1 = M.fromList (NE.toList w1)
      m2 = M.fromList (NE.toList w2)
  in NE.fromList (M.toList (M.unionWith addFrac m1 m2))

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
      -- count number of end-para (n1) and end (n2) tokens;
      -- do not want to "double count" end-para, so ignore
      -- any keys that start with EndParaToken.
      --
      -- nend should be 0; i.e. EndToken should only occur
      -- as a transition.
      ctr (n1,n2) (_,EndToken) = (n1,n2+1)
      ctr (n1,n2) (_,EndParaToken) = (n1+1,n2)
      ctr orig _ = orig
      (nep,nend) = foldl' ctr (0,0) (M.keys m)

      countEnd c ts = 
        let xs = filter (isEnd . fst) (NE.toList ts)
            isEnd EndToken = True
            isEnd _ = False
        in c + length xs
      n = M.foldl' countEnd 0 m

  in [ ("Number of keys", M.size m)
     , ("Number of start keys", NE.length (mvStart mv))
     , ("Number of end-para keys", nep)
     , ("Number of end keys (should be 0)", nend)
     , ("Number of end transitions", n)
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

