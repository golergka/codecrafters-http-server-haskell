{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CLIOptions
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Debug.Trace
import System.FilePath
import Network.Simple.TCP
  ( HostPreference (Host),
    Socket,
    recv,
    sendLazy,
    serve,
  )
import Options.Applicative
import Formatter (formatResponse)
import Parser (parseRequest)
import Types

byteLimit :: Int
byteLimit = 1024 * 10 -- 10KB

sendResponse :: Response -> Socket -> IO ()
sendResponse response serverSocket =
  sendLazy serverSocket $ BSC.fromStrict $ formatResponse response

type EndpointHandler = Request -> Path -> IO Response

handleEcho :: EndpointHandler
handleEcho _ path = return $ makeTextResponse $ join path
  where
    join = BSC.intercalate "/"

getUserAgent :: Request -> Maybe BSC.ByteString
getUserAgent request = lookup "User-Agent" (requestHeaders request)

handleUserAgent :: EndpointHandler
handleUserAgent request _ =
  return $ case getUserAgent request of
    Nothing -> makeErrorResponse "No user-agent header found"
    Just userAgent -> makeTextResponse userAgent

handleFile :: FilePath -> EndpointHandler
handleFile dir _ path =
  let filePath = dir </> joinPath (map BSC.unpack path)
   in if ".." `elem` path
        then return $ makeErrorResponse "Invalid path"
        else do
          fileContent <- BS.readFile filePath
          return $ makeTextResponse fileContent

getResponse :: CLIOptions -> Maybe BSC.ByteString -> IO Response
getResponse _ Nothing = return internalErrorResponse
getResponse options (Just request) =
  case parseRequest request of
    Left err ->
      trace ("Failed to parse request: " <> err) return internalErrorResponse
    Right parsedRequest ->
      trace ("Parsed request: " <> show parsedRequest) $
        case requestPath parsedRequest of
          [] -> return emptyResponse
          ["user-agent"] -> handleUserAgent parsedRequest []
          ("echo" : inputs) | not (null inputs) -> handleEcho parsedRequest inputs
          ("files": inputs ) | not (null inputs) -> 
            case directory options of
              Nothing -> return notFoundResponse
              Just dir -> handleFile dir parsedRequest inputs
          _ -> return notFoundResponse

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (cliOptions <**> helper)
        ( fullDesc
            <> progDesc "Run a simple HTTP server"
            <> header "http-server - a simple HTTP server"
        )

run :: CLIOptions -> IO ()
run parsedOptions = do
  BSC.putStrLn "Server is starting..."

  case directory parsedOptions of
    Nothing -> BSC.putStrLn "No directory to serve files specified"
    Just dir -> BSC.putStrLn $ "Serving directory " <> BSC.pack dir

  let host = "127.0.0.1"
      port = "4221"

  BSC.putStrLn $ "Listening on " <> BSC.pack host <> ":" <> BSC.pack port

  serve (Host host) port $ \(serverSocket, serverAddr) -> do
    BSC.putStrLn $ "Accepted connection from " <> BSC.pack (show serverAddr) <> "."

    recv serverSocket byteLimit >>= \receivedBytes -> do
      response <- getResponse parsedOptions receivedBytes
      sendResponse response serverSocket