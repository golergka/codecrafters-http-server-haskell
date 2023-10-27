{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BSC
import Network.Simple.TCP
    ( recv, sendLazy, serve, Socket, HostPreference(Host) )

import Types (Header (..), Response(..), requestPath)
import Parser (parseRequest)
import Formatter (formatResponse)

byteLimit :: Int
byteLimit = 1024 * 10 -- 10KB

sendResponse :: Response -> Socket -> IO ()
sendResponse response serverSocket =
  sendLazy serverSocket $ BSC.fromStrict $ formatResponse response

sendInternalErrorResponse :: Socket -> IO ()
sendInternalErrorResponse = sendResponse $ Response 500 "Internal Server Error" [] ""

sendNotFoundResponse :: Socket -> IO ()
sendNotFoundResponse = sendResponse $ Response 404 "Not Found" [] ""

sendOKEmptyResponse :: Socket -> IO ()
sendOKEmptyResponse = sendResponse $ Response 200 "OK" [] ""

sendOKTextResponse :: String -> Socket -> IO ()
sendOKTextResponse content = sendResponse $ Response 200 "OK" headers (BSC.pack content)
  where
    contentTypeHeader = Header "Content-Type" "text/plain"
    contentLengthHeader = Header "Content-Length" (BSC.pack $ show $ length content)
    headers = [contentTypeHeader, contentLengthHeader]

handleEcho :: [BSC.ByteString] -> Socket -> IO ()
handleEcho inputs = sendOKTextResponse $ joinAndUnpack inputs
  where
    joinAndUnpack = BSC.unpack . BSC.intercalate "/"

handleResponse :: Maybe BSC.ByteString -> IO (Socket -> IO ())
handleResponse Nothing = return sendInternalErrorResponse
handleResponse (Just request) = do
  BSC.putStrLn $ "Received request:\n" <> request
  case parseRequest request of
    Left err -> do
      BSC.putStrLn $ "Failed to parse request: " <> BSC.pack err
      return sendInternalErrorResponse
    Right parsedRequest -> do
      BSC.putStrLn $ "Parsed request: " <> BSC.pack (show parsedRequest)
      case requestPath parsedRequest of
        [] -> return sendOKEmptyResponse
        ("echo" : inputs) | not (null inputs) -> return $ handleEcho inputs
        _ -> return sendNotFoundResponse

main :: IO ()
main = do
  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  BSC.putStrLn "Server is starting..."

  -- Uncomment this block to pass first stage
  let host = "127.0.0.1"
      port = "4221"
  --
  BSC.putStrLn $ "Listening on " <> BSC.pack host <> ":" <> BSC.pack port

  serve (Host host) port $ \(serverSocket, serverAddr) -> do
    BSC.putStrLn $ "Accepted connection from " <> BSC.pack (show serverAddr) <> "."

    recv serverSocket byteLimit >>= \receivedBytes -> do
      action <- handleResponse receivedBytes
      action serverSocket