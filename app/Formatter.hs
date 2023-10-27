{-# LANGUAGE OverloadedStrings #-}

module Formatter (formatResponse) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Types (Header, Response (..))

crlf :: BSL.ByteString
crlf = "\r\n"

formatHeader :: Header -> BSL.ByteString
formatHeader (k, v) = key <> ": " <> value <> crlf
  where
    key = BSL.fromStrict k
    value = BSL.fromStrict v

formatResponse :: Response -> BSL.ByteString
formatResponse response =
  BSL.concat
    [ statusLine,
      headerLines,
      crlf,
      responseBody response
    ]
  where
    statusMessage = BSL.fromStrict $ responseStatusMessage response
    statusCode = BSCL.pack $ show $ responseStatusCode response
    statusLine = "HTTP/1.1 " <> statusCode <> " " <> statusMessage <> crlf
    headerLines = BSL.concat $ map formatHeader $ responseHeaders response