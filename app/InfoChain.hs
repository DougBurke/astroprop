{-

This code is placed in the Public Domain.

Usage:

  infochain <chain>

Aim:

Report some basic information about the chain.

-}

module Main where

-- import qualified Data.Text.IO as T

import qualified Chain as C

import Data.Functor ((<$>))
import Data.Monoid ((<>))

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)

-- | Command-line arguments.
--
data Args = Args { argChain :: String }

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "chain")

progText :: String
progText = "Display some basic information about a chain created by makechain."

helpText :: String -> String
helpText prog = prog ++ " - info on a chain."

dumpMarkov :: C.Markov -> IO ()
dumpMarkov m = do
  let dump (k,v) = putStrLn (k ++ ": " ++ show v)
  mapM_ dump (C.infoMarkov m)

process :: Args -> IO ()
process args = do
  emarkov <- C.readMarkov (argChain args)
  case emarkov of
    Left emsg -> putStrLn emsg >> exitFailure

    Right markov -> putStrLn ("Chain: " ++ argChain args) >> dumpMarkov markov

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

