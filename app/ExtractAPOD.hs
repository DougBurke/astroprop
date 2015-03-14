-- do not use OverloadedStrings as this interacts with
-- the StringLike type class from TagSoup

{-

This code is placed in the Public Domain.

Usage:
  extractapod <infile> <outhead>

Aim:
  Extract text from infile, which is expected to be a 
  HTML APOD page, created by getapod.

  Two output files are created:
    <outhead>.title
    <outhead>.text

  Any paths within <outhead> are expected to exist.

-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LB

import Control.Monad (when)

import Data.Monoid (mempty)

import Options.Applicative

import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)

import Text.HTML.TagSoup

-- | Command-line argument.
--
data Args = Args { infileArg :: FilePath
                 , outheadArg :: FilePath }

argParser :: Parser Args
argParser = Args 
   <$> argument str ( metavar "infile" )
   <*> argument str ( metavar "outhead" )

progText :: String
progText = concat [
  "Convert the HTML APOD page into two text files: <outhead>.title "
  , "and <outhead>.abstract. Any path elements in <outhead> are "
  , "expected to exist."
  ]

helpText :: String -> String
helpText prog = prog ++ " - convert APOD pages."

-- | Drop all elements that match the function, and then
--   drop the next one.
--
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 f = drop 1 . dropWhile f

titleT, centerT, bT, spaceT :: T.Text
titleT = T.pack "title"
centerT = T.pack "center"
bT = T.pack "b"
spaceT = T.pack " "

-- | What is the title.
findTitle :: [Tag T.Text] -> Maybe T.Text
findTitle tags = 
  let xs = dropWhile1 (not . isTagOpenName titleT) tags
      ys = takeWhile  (not . isTagCloseName titleT) xs
      zs = filter isTagText ys -- only expect one
      tall = T.strip (T.intercalate spaceT (map fromTagText zs))
      -- expect a title like 'APOD: YYYY Month DD - <Actual Text>'
      title = T.unwords (drop 5 (T.words tall))
  in if null zs then Nothing else Just title
 
-- | What is the piece, trying to break up into sensible paragraphs.
findText :: [Tag T.Text] -> T.Text
findText allTags =
  let start t = t ~== " Explanation: " ||
                t ~== "Explanation:" ||
                t ~== "Explanation: " ||
                t ~== " Explanation:" ||
                t ~== " Explanation " ||
                t ~== "Explanation " ||
                t ~== " Explanation" ||
                t ~== "Explanation" 
      notEnd t = t ~/= " Tomorrow's picture: " &&
                 t ~/= " Follow APOD on: " &&
                 not (center t)

      center = isTagOpenName centerT
      b = isTagCloseName bT

      xs1 = dropWhile1 (not . start) allTags
      xs2 = dropWhile1 (not . b) xs1
                 
      tags = takeWhile notEnd xs2

  in LT.toStrict (combineTags tags)

-- | Try and provide nice formatting to the text.
--
--   Can I jut process tokens individually, or is
--   some sort of state machine needed?
--   Also, is this any different from
--      innerText . findTextTags
--   ? At the moment, I don't think it is.
--
combineTags :: [Tag T.Text] -> LT.Text
combineTags = go mempty
  where 
    -- At the moment this is just foldl'/filter.
    -- Should I note TagOpen for "non expected" tags
    go b [] = LB.toLazyText b
    go b (TagText t:xs) = go (b <> LB.fromText t) xs
    go b (_:xs) = go b xs

{-
findText :: [Tag T.Text] -> T.Text
findText = innerText . drop 1 . findTextTags
-}

-- | Extract the relevant text parts of the APOD HTML page
--   stored in the input file.
--
convert :: 
  FilePath   -- ^ input file
  -> FilePath -- ^ output files are this + ".title"/".text"
  -> IO ()
convert fname ohead = do
  -- Some APOD files contain invalid sequences, which
  -- Data.Text.IO.readFile errors out on, so I need to do
  -- the conversion manually.
  -- Not entirely sure what the best solution is at this time.
  -- For now, replace the "invalid" character with a space
  -- character. This is technically unsafe - e.g. see
  -- http://unicode.org/reports/tr36/#Deletion_of_Noncharacters
  -- - but should not be an issue for the input and use case
  -- here.
  --
  apodBS <- B.readFile fname
  let -- apod = TE.decodeUtf8With TE.lenientDecode apodBS
      apod = TE.decodeUtf8With missing apodBS
      missing _ _ = Just ' '
      tags = parseTags apod
      mtitle = findTitle tags
      txt = findText tags

      writeIt ext cts = let out = ohead <.> ext
                        in T.writeFile out cts
                           >> putStrLn ("Created: " ++ out)

      rep msg = hPutStrLn stderr msg >> exitFailure

  when (T.null txt) (rep ("ERROR: no text found in " ++ fname))

  case mtitle of
    Nothing -> rep ("ERROR: unable to find title in: " ++ fname)
    Just title -> writeIt "title" (title `T.snoc` '\n')

  writeIt "text" txt

main :: IO ()
main = do
  pName <- getProgName
  let opts = info (helper <*> argParser)
                  ( fullDesc 
                    <> progDesc progText
                    <> header (helpText pName) )

  args <- execParser opts
  convert (infileArg args) (outheadArg args)
