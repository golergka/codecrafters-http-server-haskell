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

sendResponse :: Int -> String -> Socket -> IO ()
sendResponse code message serverSocket = do
  let status = "HTTP/1.1 " <> BLC.pack (show code) <> " " <> BLC.pack message <> crlf
  let headers = status <> crlf
  sendLazy serverSocket headers

sendInternalErrorResponse :: Socket -> IO ()
sendInternalErrorResponse = sendResponse 500 "Internal Server Error"

sendNotFoundResponse :: Socket -> IO ()
sendNotFoundResponse = sendResponse 404 "Not Found"

sendOKResponse :: Socket -> IO ()
sendOKResponse = sendResponse 200 "OK"

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
      if path == "/"
        then sendOKResponse serverSocket
        else sendNotFoundResponse serverSocket

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