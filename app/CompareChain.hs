{-

This code is placed in the Public Domain.

Usage:

  comparechain <chain1> <chain2>

Aim:

Compare the two chain files, listing the number of common keys.

-}

module Main where

-- import qualified Data.Text.IO as T

import qualified Chain as C

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)

-- | Command-line arguments.
--
data Args = Args { argChain1 :: String 
                 , argChain2 :: String
                 }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "chain1")
  <*> argument str (metavar "chain2")

progText :: String
progText = "Report the number of common keys in the two chains."

helpText :: String -> String
helpText prog = prog ++ " - compare chains."

compareChain :: C.Markov -> C.Markov -> IO ()
compareChain m1 m2 = do
  let dump (k,v) = putStrLn (k ++ ": " ++ show v)
  mapM_ dump (C.compareMarkov m1 m2)

getChain :: FilePath -> IO C.Markov
getChain fname = do
  emarkov <- C.readMarkov fname
  case emarkov of
    Left emsg -> putStrLn ("Error reading " ++ fname) 
                 >> putStrLn emsg 
                 >> exitFailure
    Right m   -> return m

process :: Args -> IO ()
process args = do
  markov1 <- getChain (argChain1 args)
  markov2 <- getChain (argChain2 args)
  compareChain markov1 markov2

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

