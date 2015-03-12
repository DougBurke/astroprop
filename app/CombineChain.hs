{-

This code is placed in the Public Domain.

Usage:

  combinechain <out chain> <chain1> ... <chainn>

Aim:

Combine the chains, writing out the result, with a rather surprising
argument order.

-}

module Main where

import qualified Chain as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)

-- | Command-line arguments.
--
data Args = Args { argOutput :: FilePath
                 , argChains :: [FilePath] 
                   -- this should not be empty but not encoded as such
                 }


argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "output")
  <*> some (argument str (metavar "chains..."))

progText :: String
progText = "Combine multiple chains, created by makechain, into a single chain."

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

-- | This requires that there's at least one file name
process :: Args -> IO ()
process args = do
  markovs <- mapM getChain (argChains args)
  let markov = S.sconcat (NE.fromList markovs)
  C.writeMarkov markov (argOutput args)

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  (fullDesc <> progDesc progText <> header (helpText pName))    

  args <- execParser opts
  process args

