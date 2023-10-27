{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Path,
    Header,
    Request (..),
    Response (..),
    internalErrorResponse,
    notFoundResponse,
    makeErrorResponse,
    emptyResponse,
    makeTextResponse,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type Path = [BS.ByteString]

type Header = (BS.ByteString, BS.ByteString)

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

makeErrorResponse :: String -> Response
makeErrorResponse message = Response 400 "Bad Request" headers $ BSC.pack message
  where
    contentTypeHeader = ("Content-Type", "text/plain")
    contentLengthHeader = ("Content-Length", BSC.pack $ show $ length message)
    headers = [contentTypeHeader, contentLengthHeader]

emptyResponse :: Response
emptyResponse = Response 200 "OK" [] ""

makeTextResponse :: BS.ByteString -> Response
makeTextResponse content = Response 200 "OK" headers content
  where
    contentTypeHeader = ("Content-Type", "text/plain")
    contentLengthHeader = ("Content-Length", BSC.pack $ show $ BS.length content)
    headers = [contentTypeHeader, contentLengthHeader]
