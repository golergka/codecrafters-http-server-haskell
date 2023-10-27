{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (listToMaybe)
import Network.Simple.TCP

getRequestPath :: BLC.ByteString -> Maybe BLC.ByteString
getRequestPath request = extractPath =<< getFirstLine request
  where
    getFirstLine = listToMaybe . BLC.lines
    extractPath = listToMaybe . drop 1 . BLC.words

crlf :: BLC.ByteString
crlf = "\r\n"

byteLimit :: Int
byteLimit = 1024 * 10 -- 10KB

type StatusCode = Int

type StatusMessage = String

data Header = Header String String deriving (Show)

type Body = String

sendResponse :: StatusCode -> StatusMessage -> [Header] -> Body -> Socket -> IO ()
sendResponse code message headers body serverSocket =
  sendLazy serverSocket response
  where
    statusLine = "HTTP/1.1 " <> BLC.pack (show code) <> " " <> BLC.pack message <> crlf
    headerLines = BLC.concat $ map (\(Header k v) -> BLC.pack k <> ": " <> BLC.pack v <> crlf) headers
    response = BLC.concat [statusLine, headerLines, crlf, BLC.pack body]

sendInternalErrorResponse :: Socket -> IO ()
sendInternalErrorResponse = sendResponse 500 "Internal Server Error" [] ""

sendNotFoundResponse :: Socket -> IO ()
sendNotFoundResponse = sendResponse 404 "Not Found" [] ""

sendOKEmptyResponse :: Socket -> IO ()
sendOKEmptyResponse = sendResponse 200 "OK" [] ""

sendOKTextResponse :: String -> Socket -> IO ()
sendOKTextResponse content = sendResponse 200 "OK" headers content
  where
    contentTypeHeader = Header "Content-Type" "text/plain"
    contentLengthHeader = Header "Content-Length" (show $ length content)
    headers = [contentTypeHeader, contentLengthHeader]

splitPath :: BLC.ByteString -> [BLC.ByteString]
splitPath path = filter (not . BLC.null) $ BLC.split '/' path

handleResponse :: Socket -> Maybe BLC.ByteString -> IO ()
handleResponse serverSocket Nothing = sendInternalErrorResponse serverSocket
handleResponse serverSocket (Just request) = do
  BLC.putStrLn $ "Received request:\n" <> request
  case getRequestPath request of
    Nothing -> do
      BLC.putStrLn "Failed to parse request path."
      sendInternalErrorResponse serverSocket
    Just path -> do
      BLC.putStrLn $ "Request path: " <> path
      case splitPath path of
        [] -> sendOKEmptyResponse serverSocket
        ("echo" : inputs) | not (null inputs) -> handleEcho inputs serverSocket
          where
            joinAndUnpack = BLC.unpack . BLC.intercalate "/"
            handleEcho = sendOKTextResponse . joinAndUnpack
        _ -> sendNotFoundResponse serverSocket

main :: IO ()
main = do
  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  BLC.putStrLn "Server is starting..."

  -- Uncomment this block to pass first stage
  let host = "127.0.0.1"
      port = "4221"
  --
  BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port

  serve (Host host) port $ \(serverSocket, serverAddr) -> do
    BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."

    recv serverSocket byteLimit >>= handleResponse serverSocket . fmap BLC.fromStrict