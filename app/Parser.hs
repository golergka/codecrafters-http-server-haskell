{-# LANGUAGE OverloadedStrings #-}

module Parser (parseRequest) where

import Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Word (Word8)
import Types (Header (..), Path, Request (..))

pathParser :: Parser Path
pathParser = do
  _ <- AC.char '/' <?> "Initial forward slash"
  pathComponents <- separateComponents <?> "Path components"
  return $ removeEmptyComponents pathComponents
  where
    pathBoundary c = c == '/' || c == ' '
    separateComponents = A.sepBy (AC.takeTill pathBoundary) (AC.char '/')
    removeEmptyComponents = filter $ not . BS.null

headerParser :: Parser Header
headerParser = do
  key <- AC.takeTill (== ':') <* AC.char ':' <* AC.skipSpace <?> "Header key and colon"
  value <- A.takeTill AC.isEndOfLine <* AC.endOfLine <?> "Header value and end of line"
  return $ Header key value

isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 = AC.isSpace . chr . fromIntegral

requestParser :: Parser Request
requestParser = do
  method <- A.takeTill isSpaceWord8 <* AC.space <?> "HTTP method"
  path <- pathParser <* AC.space <?> "Path"
  _ <- A.takeTill AC.isEndOfLine <* AC.endOfLine <?> "HTTP version"
  headers <- A.many' headerParser <?> "Headers"
  -- TODO parse request body
  return $ Request method path headers ""

parseRequest :: BS.ByteString -> Either String Request
parseRequest = A.parseOnly requestParser