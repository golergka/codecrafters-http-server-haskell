{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Path,
    Header (..),
    Request (..),
    Response (..),
    internalErrorResponse,
    notFoundResponse,
    emptyResponse,
    makeTextResponse
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type Path = [BS.ByteString]

data Header = Header BS.ByteString BS.ByteString deriving (Show)

data Request = Request
  { requestMethod :: BS.ByteString,
    requestPath :: Path,
    requestHeaders :: [Header],
    requestBody :: BS.ByteString
  }
  deriving (Show)

data Response = Response
  { responseStatusCode :: Int,
    responseStatusMessage :: BS.ByteString,
    responseHeaders :: [Header],
    responseBody :: BS.ByteString
  }

internalErrorResponse :: Response
internalErrorResponse = Response 500 "Internal Server Error" [] ""

notFoundResponse :: Response
notFoundResponse = Response 404 "Not Found" [] ""

emptyResponse :: Response
emptyResponse = Response 200 "OK" [] ""

makeTextResponse :: String -> Response
makeTextResponse content = Response 200 "OK" headers $ BSC.pack content
  where
    contentTypeHeader = Header "Content-Type" "text/plain"
    contentLengthHeader = Header "Content-Length" $ BSC.pack $ show $ length content
    headers = [contentTypeHeader, contentLengthHeader]
