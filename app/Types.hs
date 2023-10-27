{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Path,
    Header,
    HttpMethod (..),
    Request (..),
    Response (..),
    internalErrorResponse,
    notFoundResponse,
    methodNotAllowedResponse,
    makeErrorResponse,
    emptyResponse,
    createdResponse,
    makeTextResponse,
    makeOctetStreamResponse,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL

type Path = [BS.ByteString]

type Header = (BS.ByteString, BS.ByteString)

data HttpMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | TRACE | CONNECT
  deriving (Show)

data Request = Request
  { requestMethod :: HttpMethod,
    requestPath :: Path,
    requestHeaders :: [Header],
    requestBody :: BS.ByteString
  }
  deriving (Show)

data Response = Response
  { responseStatusCode :: Int,
    responseStatusMessage :: BS.ByteString,
    responseHeaders :: [Header],
    responseBody :: BSL.ByteString
  }

internalErrorResponse :: Response
internalErrorResponse = Response 500 "Internal Server Error" [] ""

notFoundResponse :: Response
notFoundResponse = Response 404 "Not Found" [] ""

methodNotAllowedResponse :: Response
methodNotAllowedResponse = Response 405 "Method Not Allowed" [] ""

makeErrorResponse :: String -> Response
makeErrorResponse message = Response 400 "Bad Request" headers $ BSCL.pack message
  where
    contentTypeHeader = ("Content-Type", "text/plain")
    contentLengthHeader = ("Content-Length", BSC.pack $ show $ length message)
    headers = [contentTypeHeader, contentLengthHeader]

emptyResponse :: Response
emptyResponse = Response 200 "OK" [] ""

createdResponse :: Response
createdResponse = Response 201 "Created" [] ""

makeTextResponse :: BS.ByteString -> Response
makeTextResponse content = Response 200 "OK" headers $ BSCL.fromStrict content
  where
    contentTypeHeader = ("Content-Type", "text/plain")
    contentLengthHeader = ("Content-Length", BSC.pack $ show $ BS.length content)
    headers = [contentTypeHeader, contentLengthHeader]

makeOctetStreamResponse :: BSL.ByteString -> Response
makeOctetStreamResponse content = Response 200 "OK" headers content
  where
    contentTypeHeader = ("Content-Type", "application/octet-stream")
    contentLengthHeader = ("Content-Length", BSC.pack $ show $ BSL.length content)
    headers = [contentTypeHeader, contentLengthHeader]