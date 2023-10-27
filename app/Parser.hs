{-# LANGUAGE OverloadedStrings #-}

module Parser (parseRequest) where

import Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import Types (Header, HttpMethod (..), Path, Request (..))

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
  return (key, value)

httpMethodParser :: Parser HttpMethod
httpMethodParser =
  choice
    [ string "GET" >> return GET,
      string "POST" >> return POST,
      string "PUT" >> return PUT,
      string "DELETE" >> return DELETE,
      string "PATCH" >> return PATCH,
      string "HEAD" >> return HEAD,
      string "OPTIONS" >> return OPTIONS,
      string "TRACE" >> return TRACE,
      string "CONNECT" >> return CONNECT
    ]

requestParser :: Parser Request
requestParser = do
  method <- httpMethodParser <* AC.space <?> "HTTP method"
  path <- pathParser <* AC.space <?> "Path"
  _ <- A.takeTill AC.isEndOfLine <* AC.endOfLine <?> "HTTP version"
  headers <- A.many' headerParser <?> "Headers"
  _ <- AC.endOfLine <?> "End of headers"
  body <- A.takeByteString <?> "Request body"
  return $ Request method path headers body

parseRequest :: BS.ByteString -> Either String Request
parseRequest = A.parseOnly requestParser