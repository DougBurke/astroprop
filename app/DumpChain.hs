{-

This code is placed in the Public Domain.

Usage:

  dumpchain <chain>

Aim:

Write out the contents of the chain to the screen, in a non-optimised form:
  
  <key1>\t<key2>\t<word>\t<n>

That is, each (Key, (Word,Count)) tuple is displayed on a single line.

-}

module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Chain as C

import Data.Ratio (numerator)

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
progText = "Dump the contents of the chain as tab-separated <key1 key2 word count> values (one per line)."

helpText :: String -> String
helpText prog = prog ++ " - dump the chain."

tab :: T.Text
tab = T.pack "\t"

-- AHA: what do I do about "end of para" tokens, since I do not want the
--      new line included in the token, but want some identifier.
--
dumpMarkov :: C.Markov -> IO ()
dumpMarkov m = do
  let dump ((k1,k2), vs) = 
        -- I assume small enough strings that using a builder is not worth it
        let hdr = C.dumpToken k1 <> tab <> C.dumpToken k2 <> tab
        in mapM_ (dumpv hdr) vs

      -- going to assume that the denominator is always 1; it should be
      fromN = T.pack . show . numerator
      dumpv hdr (v,n) = T.putStrLn (hdr <> C.dumpToken v <> tab <> fromN n)

      mmap = M.toList (C.getTransitions m)

  mapM_ dump mmap

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

