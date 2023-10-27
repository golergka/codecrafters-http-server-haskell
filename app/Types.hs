module Types (Path, Header (..), Request (..), Response (..)) where

import qualified Data.ByteString as BS

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