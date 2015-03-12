{-

This code is placed in the Public Domain.

Usage:

  combinechain <chain1> <chain2> <out chain>

Aim:

Combine the two chains, writing out the result.

-}

module Main where

import qualified Chain as C

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)

-- | Command-line arguments.
--
data Args = Args { argChain1 :: FilePath
                 , argChain2 :: FilePath
                 , argOutput :: FilePath
                 }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "chain1")
  <*> argument str (metavar "chain2")
  <*> argument str (metavar "output")

progText :: String
progText = "Combine the two chains."

helpText :: String -> String
helpText prog = prog ++ " - combine chains."

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
  let markov = C.combineMarkov markov1 markov2
  C.writeMarkov markov (argOutput args)

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

