{-# LANGUAGE OverloadedStrings #-}

{-

This code is placed in the Public Domain.

Usage:
  getproposals <telescope id> <outdir>
     --start <start>
     --nrows <nrows>

Aim:
  Search NASA ADS for (successful) proposals from the given
  telescope (using the appropriate "bibstem" field, such as
  "cxc..prop" for Chandra), and write the results to

    <outdir>/title.<bibcode>
    <outdir>/abstract.<bibcode>

  The <start> argument gives the number to start with and <nrows>
  the number of entries to retrieve (see your ADS API limits for
  the maximum value allowed).

  The telescope name is assumed to only contain ascii characters,
  i.e. those that have Unicode code points less than 256.

  Your ADS API key must be stored in the file dev_key.txt in the
  current working directory when the tool is run.

Issues:

  It is not particularly clever if any record does not contain
  all the desired fields, since this causes the program to exit,
  rather than trying to do something like parse the records that
  are correct.
-}

module Main where

import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.IO.Strict as S

import Control.Monad.State.Strict (evalStateT)

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..)
                   , (.:), (.=)
                   , json', object)

import Options.Applicative

import Pipes
import Pipes.Attoparsec (parse)
import Pipes.HTTP

import System.Environment (getProgName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), addExtension)
import System.IO.Error (catchIOError, ioeGetErrorString)

newtype ADSKey = ADSKey { _adsKey :: String }

-- | Location of the key file
keyFile :: FilePath
keyFile = "dev_key.txt"

-- | Read the key from the file `keyFile`.
getADSKey :: IO (Either String ADSKey)
getADSKey = 
  let toError ioe = "Error reading from " ++ keyFile ++ ": " ++ 
                    ioeGetErrorString ioe
  in catchIOError (do
    cts <- S.readFile keyFile
    return $ case words cts of
      [] -> Left (keyFile ++ " is empty")
      (k:_) -> Right (ADSKey k)  
    ) (return . Left . toError)

-- | Command-line arguments.
--
data Args = Args { telescope :: String
                 , outdir :: FilePath
                 , start :: Int
                 , nrows :: Int
                 } 

argParser :: Parser Args
argParser = Args 
   <$> argument str 
       ( metavar "telescope" )
   <*> argument str 
       ( metavar "outdir" )
   <*> option auto 
       ( long "start"
      <> value 0
      <> showDefault
      <> help "What number to start search at, starting at 0" )
   <*> option auto 
       ( long "nrows"
      <> value 1
      <> showDefault
      <> help "Number of rows" )

progText :: String
progText = concat [
  "The telescope argument is the NASA/ADS bibstem value, e.g. cxc..prop. "
  , "The outdir argument is the output directory to store the abstracts "
  , "and titles (this will be created if it does not exist)."
  , "\n"
  , "The ADS key is read from the file dev_key.txt (the first word in the "
  , "file is used)."
  ]

helpText :: String -> String
helpText prog = prog ++ " - grab proposal abstracts and titles from ADS."

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  ( fullDesc 
                    <> progDesc progText
                    <> header (helpText pName) )

  args <- execParser opts
  ekey <- getADSKey
  case ekey of
    Left emsg -> putStrLn $ "ERROR: " ++  emsg
    Right key -> runSearch args key

runSearch :: Args -> ADSKey -> IO ()
runSearch args key = do
  req <- parseUrl "http://adslabs.org/adsabs/api/search/"
  let req' = setQueryString qopts req 
      qopts = [ ("q", Just bibstemBS)
              , ("start", Just startBS)
              , ("rows", Just rowBS)
              , ("dev_key", Just keyBS)
              ]
      keyBS = B8.pack $ _adsKey key
      startBS = B8.pack $ show (start args)
      rowBS = B8.pack $ show (nrows args)
      bibstemBS = B8.pack $ "bibstem:" ++ telescope args

  createDirectoryIfMissing True (outdir args)
  withManager defaultManagerSettings $ \mgr ->
    withHTTP req' mgr $ \resp -> do
      mres <- evalStateT (parse json') (responseBody resp)
      case mres of
        Just (Right json) -> writeResponses (outdir args) json
        Just (Left pe) -> putStrLn ("Failed parsing: " ++ show pe)
        _ -> putStrLn "Failed somewhere"

writeResponses :: FilePath -> Value -> IO ()
writeResponses odir json = 
  case AT.fromJSON json of
    AT.Error emsg -> putStrLn $ "Error: " ++ emsg
    AT.Success ads -> mapM_ (writeResponse odir) (adsEntities ads)

-- Note: overwrites existing output
writeResponse :: FilePath -> ADSEntity -> IO ()
writeResponse odir (ADSEntity bibCode title abstract) = do
  let outHead = odir </> T.unpack bibCode
      
  T.putStrLn bibCode

  let writeIt ext cts =
       let fname = outHead `addExtension` ext
       in T.writeFile fname cts
 
  writeIt "title" title
  writeIt "abstract" abstract
      
{-

The ADS JSON response is expected to look like

{  "meta": {"api-version": "0.1.1", "count": 1, "hits": 4727, "qtime": 80, "query": "bibstem:cxo"}, 
  "results": 
    {"docs": 
     [
      {"bibcode": "2014cxo..prop.4536C", "pubdate": "2014-09-00", "author": ["Canizares, Claude"], "abstract": "The direct measurement of an unbiased and Si K edge optical depth and possible structure in the absorption regime between 1021 and 1022 cm-2 is complex and difficult and needs a specific instrument setup. We therefore propose a 135 ksec observation of Ser X-1 in TE mode and a very tight subarray of 134 rows and only two grating arm on the array. For this only the S3 and S2 devices need to be on and we cover the Si K edge on S3, which as a back-illuminated device is devoid of instrumental edge contributions.", "database": ["astronomy"], "pub": "Chandra Proposal", "[citations]": {"num_citations": 0, "num_references": 0}, "property": ["NONARTICLE", "NOT REFEREED"], "aff": ["-"], "year": "2014", "title": ["Measure an unbiased Si K edge optical depth in Ser X-1"], "identifier": ["2014cxo..prop.4536C"], "id": "2058457", "page": ["4536"]
      }
     ]
    }
}

So, we want to extract the docs field of the results entry,
and parse as an array of objects, extracting the bibcode, abstact, and title
values.

  bibcode is a string
  abstract is a string
  title is an array: why?

Could look at the meta.count field and check not 0.
 
-}

-- | Represent a single abstract/record.
--
data ADSEntity = ADSEntity {
    adsBibCode :: T.Text
    , adsTitle :: T.Text
    , adsAbstract :: T.Text
    }

instance FromJSON ADSEntity where
  parseJSON (Object o) = 
    ADSEntity <$> o .: "bibcode"
              <*> (head <$> o .: "title") -- assume first entry from the list
              <*> o .: "abstract"
  
  parseJSON _ = mzero

instance ToJSON ADSEntity where
  toJSON a = object [ "bibcode" .= adsBibCode a
                    , "title" .= [adsTitle a]
                    , "abstract" .= adsAbstract a
                    ]

-- | Extract all the data from an ADS response.
--
data ADSResponse = ADSResponse { adsEntities :: [ADSEntity] }

instance FromJSON ADSResponse where
  parseJSON (Object o) = do
    docs <- o .: "results" >>= (.: "docs")
    ADSResponse <$> parseJSON docs
  
  parseJSON _ = mzero

