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

Example ADS/API output:

% curl -H "Authorization: Bearer:<magic token>" "https://api.adsabs.harvard.edu/v1/search/query?q=bibstem:cxo..prop&start=1&rows=1&fl=abstract,bibcode,author,data,doi,keyword,orcid_pub,orcid_user,orcid_other,title,vizier"
{"responseHeader":{"status":0,"QTime":42,"params":{"q":"bibstem:cxo..prop","fl":"abstract,bibcode,author,data,doi,keyword,orcid_pub,orcid_user,orcid_other,title,vizier","start":"1","rows":"1","wt":"json"}},"response":{"numFound":4441,"start":1,"docs":[{"title":["ACIS Observations of Jupiter"],"abstract":"The intent of these 4 ACIS observations of Jupiter is to investigate the best strategy for carrying out solar system observations which are complicated by the sensitivity of the ACIS-S array to red light. The first of these observations will aquire a bias map with Jupiter outside of the field of view of ACIS-S. The following three observations will be carried out with different instrument settings (primarly the event and split thresholds) and no bias map. A slew must be perfomed between all four observations to locate Jupiter on the detector since it will drift during the required 3 ksec exposures. The exact pointings will be filled in at a later time once the the observing date is known.","data":["CXO"],"bibcode":"2000cxo..prop..720C","keyword":["Chandra Proposal ID #02108032"],"author":["CXC Calibration"],"orcid_pub":["-"]}]}}


-}

module Main where

import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.IO.Strict as S

import Control.Monad.State.Strict (evalStateT)

import Data.Aeson (FromJSON(..)
                   , Value(..)
                   , (.:), (.:?)
                   , json')
import Data.Functor ((<$>))
import Data.Monoid ((<>))

import Network.HTTP.Client.TLS (tlsManagerSettings)

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
  let req = parseRequest_ "https://api.adsabs.harvard.edu/v1/search/query"

      -- add in ADS authorization
      ohdrs = requestHeaders req
      authorize = ("Authorization", "Bearer:" <> keyBS)
      keyBS = B8.pack (_adsKey key)
      nhdrs = authorize : ohdrs
      req1 = req { requestHeaders = nhdrs }

      -- Note: the keyword field is expected to contain
      -- "Chandra Proposal ID #<n>".
      --
      fieldNames = "title,abstract,bibcode,keyword,author"
  
      req2 = setQueryString qopts req1 
      qopts = [ ("q", Just bibstemBS)
              , ("start", Just startBS)
              , ("rows", Just rowBS)
              , ("fl", Just fieldNames)
              ]
      startBS = B8.pack $ show (start args)
      rowBS = B8.pack $ show (nrows args)
      bibstemBS = B8.pack $ "bibstem:" ++ telescope args

  createDirectoryIfMissing True (outdir args)

  -- is this sufficient to get TLS working?
  mgr <- newManager tlsManagerSettings
  withHTTP req2 mgr $ \resp -> do
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
writeResponse odir (ADSEntity bibCode title mabstract) = do
  let outHead = odir </> T.unpack bibCode
      
  T.putStrLn bibCode

  let writeIt ext cts =
       let fname = outHead `addExtension` ext
       in T.writeFile fname cts
 
  writeIt "title" title
  case mabstract of
    Just abstract -> writeIt "abstract" abstract
    _ -> putStrLn "  -- no abstract"
  
{-

The ADS JSON response is expected to look like (fl and returned fields may be
slightly different)

{
  "responseHeader": {
    "status":0, "QTime":42, "params":{"q":"bibstem:cxo..prop","fl":"abstract,bibcode,author,data,doi,keyword,orcid_pub,orcid_user,orcid_other,title,vizier","start":"1","rows":"1","wt":"json"}
  },
  "response": {
    "numFound":4441,
    "start":1,
    "docs":[ {
      "title":["ACIS Observations of Jupiter"],
      "abstract":"The intent of these 4 ACIS observations of Jupiter is to investigate the best strategy for carrying out solar system observations which are complicated by the sensitivity of the ACIS-S array to red light. The first of these observations will aquire a bias map with Jupiter outside of the field of view of ACIS-S. The following three observations will be carried out with different instrument settings (primarly the event and split thresholds) and no bias map. A slew must be perfomed between all four observations to locate Jupiter on the detector since it will drift during the required 3 ksec exposures. The exact pointings will be filled in at a later time once the the observing date is known.",
      "data":["CXO"],
      "bibcode":"2000cxo..prop..720C",
      "keyword":["Chandra Proposal ID #02108032"],
      "author":["CXC Calibration"],
      "orcid_pub":["-"]}]}}

-}

-- | Represent a single abstract/record.
--
--   It appears that some abstracts can be missing
data ADSEntity = ADSEntity {
    adsBibCode :: T.Text
    , adsTitle :: T.Text
    , adsAbstract :: Maybe T.Text
    }

instance FromJSON ADSEntity where
  parseJSON (Object o) = 
    ADSEntity <$> o .: "bibcode"
              <*> (head <$> o .: "title") -- assume first entry from the list
              <*> o .:? "abstract"
  
  parseJSON _ = mzero

-- | Extract all the data from an ADS response.
--
data ADSResponse = ADSResponse { adsEntities :: [ADSEntity] }

instance FromJSON ADSResponse where
  parseJSON (Object o) = do
    docs <- o .: "response" >>= (.: "docs")
    ADSResponse <$> parseJSON docs
  
  parseJSON _ = mzero

