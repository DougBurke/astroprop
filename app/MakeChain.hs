{-

This code is placed in the Public Domain.

Usage:

  makechain <file regexp/glob> <out>

Aim:

Train a Markov Chain (order 2) on the files in the given
glob, and write the output to the file <out>. This output
file can then be used by runchain.

-}

module Main where

import qualified Data.Text.IO as T

import qualified Chain as C

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.FilePath.Glob (glob)

-- | Command-line arguments.
--
data Args = Args { argGlob :: String
                 , argOut :: FilePath
                 }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "glob")
  <*> argument str (metavar "out")

progText :: String
progText = "Train a Markov chain on the input data, and write out the "
           ++ "results for use by runchain."

helpText :: String -> String
helpText prog = prog ++ " - train a Markov chain."

process :: Args -> IO ()
process args = do
  files <- glob (argGlob args)
  putStrLn $ "Reading files: " ++ argGlob args
  cts <- mapM T.readFile files
  let allToks = map C.tokenize cts

  case C.buildMarkov allToks of
    Just markov -> do
      putStrLn $ "Writing chain: " ++ argOut args
      C.writeMarkov markov (argOut args)

    _ -> putStrLn "*** No capital start words found in training set"
         >> exitFailure

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

