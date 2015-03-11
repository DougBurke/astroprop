{-# LANGUAGE DeriveGeneric #-}

{-

This code is placed in the Public Domain.

Library code for training a Markov chain on data
and then using it to create gibberish.

At present it uses an order 2 chain, using "words"
as the token type.

This module defines orphan instances.
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

       , getSeed
       ) where

import qualified Control.Monad.Random as R

import qualified Data.ByteString.Lazy as LB

-- I have done no profiling to see whether it's worth skipping
-- the default Map and jumping to unordered-containers. I just
-- did it.
--
import qualified Data.HashMap.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Arrow (second)

import Data.Char (isUpper)
import Data.Hashable (Hashable(..))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator)
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
newtype Token = Token T.Text
  deriving (Eq, Ord, Generic)

toToken :: T.Text -> Maybe Token
toToken t | T.null t  = Nothing
          | otherwise = Just (Token t)

fromToken :: Token -> T.Text
fromToken (Token t) = t

lenToken :: Token -> Int
lenToken (Token t) = T.length t

instance Hashable Token where
  hashWithSalt s (Token t) = hashWithSalt s t
  hash (Token t) = hash t

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

instance Serialize Token
instance Serialize Markov

-- For the moment use a simple tokenization strategy.
--
tokenize :: T.Text -> [Token]
tokenize = mapMaybe toToken . T.words

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
  let wanted (Token t) = case T.uncons t of
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
   
-- | This is only intended for short output sequences.
--
buildChain :: R.MonadRandom m => Int -> Int -> Markov -> Key -> m [Token]
buildChain maxlen curlen m start = 
  case M.lookup start (mvMap m) of
    Nothing -> return []
    Just transitions -> do
      next <- R.fromList transitions
      let newlen = curlen + lenToken next + 1
      if newlen > maxlen
        then return []
        else do
          rest <- buildChain maxlen newlen m (snd start,next)
          return (next : rest)

-- Stop the chain if the word takes the total length past the
-- input len. As a simplification, the check is not applied
-- to the starting pair.
--
runMarkov :: R.MonadRandom m => Int -> Markov -> m [T.Text]
runMarkov maxlen markov = do
  start@(s1,s2) <- startToken markov
  rest <- buildChain maxlen (lenToken s1 + lenToken s2 + 1) markov start
  return $ map fromToken $ s1 : s2 : rest

-- | A version of runMarkov where the seed is explicit.
--
seedMarkov :: 
  Int  -- ^ Maximum number of characters
  -> Markov 
  -> Int  -- ^ Seed for random-number generator
  -> IO [T.Text]
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

