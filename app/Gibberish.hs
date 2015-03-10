{-

This code is placed in the Public Domain.

Usage:

  gibberish <file regexp>
    --chars <max number of chars; default is 120>
    --seed  <integer to seed the generator>

Aim:

Create a Markov Chain after training on the data
in the given input files. The current approach is
to use word pairs (Markov Chain of order 2).

-}

module Main where

import qualified Control.Monad.Random as R

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Chain as C

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

process :: Args -> IO ()
process args = do
  files <- glob (argGlob args)
  cts <- mapM T.readFile files
  let allToks = map C.tokenize cts

  case C.buildMarkov allToks of
    Just markov -> do
      let chain :: R.MonadRandom m => m [T.Text]
          chain = C.runMarkov (argLen args) markov

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
