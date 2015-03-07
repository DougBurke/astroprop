{-

Usage:

  chain <file regexp>
    --chars <max number of chars; default is 120>
    --seed  <integer to seed the generator>

Aim:

Create a Markov Chain after training on the data
in the given input files. The current approach is
to use word pairs (Markov Chain of order 2).

-}

module Main where

import qualified Control.Monad.Random as R

-- I have done no profiling to see whether it's worth skipping
-- the default Map and jumping to unordered-containers. Since
--
import qualified Data.HashMap.Strict as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Arrow (second)

import Data.Char (isUpper)
import Data.Hashable (Hashable(..))
import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Options.Applicative

import System.Environment (getProgName)
import System.FilePath.Glob (glob)
import System.Random (mkStdGen)

-- | Command-line arguments.
--
data Args = Args { argGlob :: String
                 , argLen :: Int
                 , argSeed :: Maybe Int
                 }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "glob")
  <*> option auto (long "nchar" 
                   <> value 120
                   <> showDefault
                   <> help "Number of characters")
  <*> option (Just <$> auto) (long "seed"
                   <> value Nothing
                   <> help "Seed for random-number generator (integer)")

progText :: String
progText = "Create a Markov chain trained on the input data."

helpText :: String -> String
helpText prog = prog ++ " - create a Markov chain of gibberish."

-- | The tokens to use. I go to the effort of a newtype
--   since I want to ensure that the tokens are not empty
--   strings. Tokens can include punctuation and are
--   case sensitive when comparing.
--
newtype Token = Token T.Text
  deriving (Eq, Ord)

toToken :: T.Text -> Maybe Token
toToken t | T.null t  = Nothing
          | otherwise = Just (Token t)

unToken :: Token -> T.Text
unToken (Token t) = t

tokLen :: Token -> Int
tokLen (Token t) = T.length t

instance Hashable Token where
  hashWithSalt s (Token t) = hashWithSalt s t
  hash (Token t) = hash t

-- Hand roll a simple Markov generator. I use a slightly
-- different type for running the chain than for building
-- it; as the code is currently written this seems an
-- unnescessary optimisation.
--
type Key = (Token, Token)
type TransitionWeight = (Token, Rational)
type TransitionMap = M.HashMap Token Int

type MarkovBuild = M.HashMap Key TransitionMap

type TransitionWeights = [TransitionWeight]

-- | Calculate the list of start tokens - those keys
--   for which the first token is capitalized - so that
--   the code can error out in the unlikely case there's
--   no such match.
--
data MarkovUse = MU 
  { muMap :: M.HashMap Key TransitionWeights
  , muStart :: [Key]
  }

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

convert :: MarkovBuild -> Maybe MarkovUse
convert m = 
  let wanted t = case T.uncons (unToken t) of
                   Nothing -> error "Token invariant invalidated!"
                   Just (c,_) -> isUpper c

      out = MU { muMap = M.map (map (second toRational). M.toList) m
               , muStart = filter (wanted.fst) $ M.keys m
               }

  in if null (muStart out) then Nothing else Just out

-- | Pick a start pair from the set, ensuring that the
--   first token is capitalized.
--
--   This *assumes* that there is a capitalized starting
--   token in the training set.
--
startToken :: R.MonadRandom m => MarkovUse -> m Key
startToken m = R.uniform $ muStart m
   
-- This is only intended for short output sequences.
--
buildChain :: R.MonadRandom m => Int -> Int -> MarkovUse -> Key -> m [Token]
buildChain maxlen curlen m start = 
  case M.lookup start (muMap m) of
    Nothing -> return []
    Just transitions -> do
      next <- R.fromList transitions
      let newlen = curlen + tokLen next + 1
      if newlen > maxlen
        then return []
        else do
          rest <- buildChain maxlen newlen m (snd start,next)
          return (next : rest)

-- Stop the chain if the word takes the total length past the
-- input len. As a simplification, the check is not applied
-- to the starting pair.
--
runChain :: R.MonadRandom m => Int -> MarkovUse -> m [T.Text]
runChain maxlen markov = do
  start@(s1,s2) <- startToken markov
  rest <- buildChain maxlen (tokLen s1 + tokLen s2 + 1) markov start
  return $ map unToken $ s1 : s2 : rest

process :: Args -> IO ()
process args = do
  files <- glob (argGlob args)
  cts <- mapM T.readFile files
  let allToks = map tokenize cts

  case convert (initialize allToks) of
    Just markov -> do
      let chain :: R.MonadRandom m => m [T.Text]
          chain = runChain (argLen args) markov

      toks <- case argSeed args of
        Just seed -> R.evalRandT chain (mkStdGen seed)
        _ -> R.evalRandIO chain

      T.putStrLn (T.unwords toks)

    _ -> putStrLn "*** No capital start words found in training set"

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

