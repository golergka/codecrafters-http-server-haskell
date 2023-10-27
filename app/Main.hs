{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import Formatter (formatResponse)
import Network.Simple.TCP
  ( HostPreference (Host),
    Socket,
    recv,
    sendLazy,
    serve,
  )
import Parser (parseRequest)
import Types

byteLimit :: Int
byteLimit = 1024 * 10 -- 10KB

sendResponse :: Response -> Socket -> IO ()
sendResponse response serverSocket =
  sendLazy serverSocket $ BSC.fromStrict $ formatResponse response

handleEcho :: Request -> Path -> Response
handleEcho _ path = makeTextResponse $ joinAndUnpack path
  where
    joinAndUnpack = BSC.unpack . BSC.intercalate "/"

getResponse :: Maybe BSC.ByteString -> Response
getResponse Nothing = internalErrorResponse
getResponse (Just request) =
  case parseRequest request of
    Left err ->
      trace ("Failed to parse request: " <> err) internalErrorResponse
    Right parsedRequest ->
      trace ("Parsed request: " <> show parsedRequest) $
        case requestPath parsedRequest of
          [] -> emptyResponse
          ("echo" : inputs) | not (null inputs) -> handleEcho parsedRequest inputs
          _ -> notFoundResponse

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
      let response = getResponse receivedBytes
      sendResponse response serverSocket