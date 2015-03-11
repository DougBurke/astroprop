{-

This code is placed in the Public Domain.

Usage:

  runchain <chain>
    --chars <max number of chars; default is 120>
    --seed  <integer to seed the generator>

Aim:

Take the output from makechain and create a stream
of gibberish from it.

-}

module Main where

import qualified Data.Text.IO as T

import qualified Chain as C

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)

-- | Command-line arguments.
--
data Args = Args { argChain :: String
                 , argLen :: Int
                 , argSeed :: Maybe Int
                 }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "chain")
  <*> option auto (long "nchar" 
                   <> value 120
                   <> showDefault
                   <> help "Number of characters")
  <*> option (Just <$> auto) (long "seed"
                   <> value Nothing
                   <> help "Seed for random-number generator (integer)")

progText :: String
progText = "Run a Markov Chain to create gibberish."

helpText :: String -> String
helpText prog = prog ++ " - chain together gibberish."

process :: Args -> IO ()
process args = do
  emarkov <- C.readMarkov (argChain args)
  case emarkov of
    Left emsg -> putStrLn emsg >> exitFailure

    Right markov -> do
      seed <- case argSeed args of
               Just s -> return s
               _ -> C.getSeed
 
      -- as I want to display the seed I don't use the system one 
      putStrLn $ "Seed: " ++ show seed
      gibberish <- C.seedMarkov (argLen args) markov seed
      T.putStrLn gibberish

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

