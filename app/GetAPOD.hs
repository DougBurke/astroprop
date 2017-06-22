{-

This code is placed in the Public Domain.

Usage:
  getapod <outdir>
     --index <filename>
     --npages <npage>

Aim:
  Download the HTML pages for APOD (Astronomy Picture Of the Day;
  http://apod.nasa.gov/apod/astropix.html). The idea is to
  search the archive and download any missing (new or old)
  files. There is a limit to the number of downloaded files.

  The files are written to <outdir>/ and use the same file name
  as on the APOD web site. This is used to search for missing
  files. The search is done in reverse chronological order.

  If --index is given then 
    a) if the index file exists, it is read from rather than
       downloading the APOD index page
    b) if it does not exist, then the downloaded index page is
       written out.

-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as LB8

import qualified Pipes.ByteString as PB

import qualified System.IO.Strict as S

import Control.Monad (filterM)

import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

import Options.Applicative

import Pipes
import Pipes.HTTP

import System.Environment (getProgName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(WriteMode))

import Text.HTML.TagSoup

-- | Command-line arguments.
--
data Args = Args { outdirArg :: FilePath
                 , indexArg :: Maybe FilePath
                 , npagesArg :: Int
                 } 

argParser :: Parser Args
argParser = Args 
   <$> argument str 
       ( metavar "outdir" )
   <*> option (Just <$> str)
       ( long "index"
      <> value Nothing
      <> help "Local APOD index page (read if exists, or write)" )
   <*> option auto 
       ( long "npages"
      <> value 50
      <> showDefault
      <> help "Number of pages to download" )

progText :: String
progText = concat [
  "Download pages from APOD, and store them in outdir. The number of pages "
  , "to download is given by the --npages option, and the search is "
  , "done in a reverse chronological manner (so newest first). "
  , "If the --index argument is then this is use either as the index file, "
  , "if it exists, or it is created with a copy of the APOD index page."
  ]

helpText :: String -> String
helpText prog = prog ++ " - grab APOD pages."

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  ( fullDesc 
                    <> progDesc progText
                    <> header (helpText pName) )

  args <- execParser opts
  runSearch args

-- | What to do with the index file.
data IndexAction = NoAction | SaveFile FilePath String

-- | Return the contents of the index page. May be local
--   or downloaded.
--
findIndexPage :: Maybe FilePath -> Manager -> IO (String, IndexAction)
findIndexPage (Just fname) mgr = do
  fexists <- doesFileExist fname
  if fexists 
    then S.readFile fname >>= \cts -> return (cts, NoAction)
    else readIndexPage mgr >>= \cts -> return (cts, SaveFile fname cts)

findIndexPage _ mgr = readIndexPage mgr >>= \cts -> return (cts, NoAction)

readIndexPage :: Manager -> IO String
readIndexPage mgr = do
  let req = parseRequest_ "http://apod.nasa.gov/apod/archivepix.html"
  -- not streaming here!
  LB8.unpack <$> withHTTP req mgr (PB.toLazyM . responseBody)

-- | Return the links to apod pages from the index page.
--
--   Expect
--
--   <head>...</head>
--   <body> 
--     <center>..</center> <center>..</center>
--     <b>
--       text: <a href="xxxx">title</a><br>
--       text: <a href="xxxx">title</a><br>
--       ...
--       text: <a href="xxxx">title</a><br>
--     </b>
--     ...
--   </body>
--
--   It looks like we want the first <b>/../</b>
--   pair.
--
--   This could also extract the titles for the pages,
--   but this should be in the HTML page so leave for now.
--
findPages :: String -> [String]
findPages cts = 
  let tags = parseTags cts
      btag = isTagOpenName "b"

      section = takeWhile (not.btag) (drop 1 (head (partitions btag tags)))

      getLink (TagOpen "a" attrs) = lookup "href" attrs
      getLink _ = Nothing

  in mapMaybe getLink section

-- | Download the page from APOD and place it in the directory.
--
downloadPage :: FilePath -> Manager -> String -> IO ()
downloadPage odir mgr apath = do
  putStrLn ("Downloading: " ++ apath)
  req <- parseRequest ("http://apod.nasa.gov/apod/" ++ apath)
  withHTTP req mgr $ \resp -> do
    let oname = odir </> apath
    withFile oname WriteMode $ \oh ->
      runEffect (responseBody resp >-> PB.toHandle oh)

-- | Return the names of the files that are missing; i.e. those
--   files for which outdir </> filename does not exist.
--
findMissing :: FilePath -> [String] -> IO [String]
findMissing outdir = filterM find 
  where
    find = (not <$>) . doesFileExist . (outdir </>)

runSearch :: Args -> IO ()
runSearch args = do
  let out = outdirArg args
  createDirectoryIfMissing True out
  mgr <- newManager defaultManagerSettings
  (indexPage, act) <- findIndexPage (indexArg args) mgr
  let allPages = findPages indexPage
  putStrLn ("Found " ++ show (length allPages) ++ " pages from APOD.")

  missingPages <- findMissing out allPages
  putStrLn ("There are " ++ show (length missingPages) ++ " missing pages.")

  mapM_ (downloadPage out mgr) (take (npagesArg args) missingPages)

  case act of
    NoAction -> return ()
    SaveFile fname cts -> writeFile fname cts
                          >> putStrLn ("Saved APOD index to: " ++ fname)

