{-# LANGUAGE OverloadedStrings #-}

module Formatter (formatResponse) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Types (Header, Response (..))

crlf :: BS.ByteString
crlf = "\r\n"

formatHeader :: Header -> BS.ByteString
formatHeader (k, v) = k <> ": " <> v <> crlf

formatResponse :: Response -> BS.ByteString
formatResponse response =
  BS.concat
    [ statusLine,
      headerLines,
      crlf,
      responseBody response
    ]
  where
    statusMessage = responseStatusMessage response
    statusCode = BSC.pack $ show $ responseStatusCode response
    statusLine = "HTTP/1.1 " <> statusCode <> " " <> statusMessage <> crlf
    headerLines = BS.concat $ map formatHeader $ responseHeaders response