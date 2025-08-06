{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.TLS
import Network.TLS.Extra.Cipher
import Data.Default.Class
import System.X509
import Control.Monad (forever, when, unless, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException, finally)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist, listDirectory, removeFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, takeFileName)
import Data.Time
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Text.Read (readMaybe)
import GHC.Generics
import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process (readProcess)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Crypto.Hash (hash, Digest, SHA256)
import Data.ByteArray.Encoding (convertToBase, Base(Base64))

-- Data types
data ServerConfig = ServerConfig
    { port :: String
    , certFile :: FilePath
    , keyFile :: FilePath
    , documentRoot :: FilePath
    , logFile :: FilePath
    , maxConnections :: Int
    , enableAuth :: Bool
    } deriving (Show, Generic)

instance FromJSON ServerConfig
instance ToJSON ServerConfig

data ClientSession = ClientSession
    { sessionId :: String
    , authenticated :: Bool
    , lastActivity :: UTCTime
    , clientInfo :: String
    } deriving (Show, Generic)

instance FromJSON ClientSession
instance ToJSON ClientSession

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , clientAddr :: String
    , method :: String
    , path :: String
    , status :: Int
    , message :: String
    } deriving (Show, Generic)

instance FromJSON LogEntry
instance ToJSON LogEntry

-- Global state
type SessionStore = TVar (Map.Map String ClientSession)
type LogStore = TVar [LogEntry]

-- HTTP-like response builder
buildResponse :: Int -> String -> [(String, String)] -> BS.ByteString -> BS.ByteString
buildResponse statusCode statusText headers body = 
    let statusLine = "HTTP/1.1 " ++ show statusCode ++ " " ++ statusText ++ "\r\n"
        headerLines = map (\(k, v) -> k ++ ": " ++ v ++ "\r\n") headers
        allHeaders = statusLine ++ concat headerLines ++ "\r\n"
    in C.pack allHeaders <> body

-- Parse simple HTTP-like requests
parseRequest :: BS.ByteString -> Maybe (String, String, [(String, String)], BS.ByteString)
parseRequest request = 
    let lines' = C.lines request
    in case lines' of
        [] -> Nothing
        (firstLine:rest) ->
            let parts = C.words firstLine
            in case parts of
                [method, path, _] -> 
                    let (headerLines, bodyLines) = span (/= "") rest
                        headers = map parseHeader headerLines
                        body = if null bodyLines then "" else C.intercalate "\r\n" (tail bodyLines)
                    in Just (C.unpack method, C.unpack path, headers, body)
                _ -> Nothing
  where
    parseHeader line = 
        let (key, rest) = C.break (== ':') line
        in (C.unpack key, C.unpack $ C.dropWhile (== ' ') $ C.drop 1 rest)

-- File serving functionality
serveFile :: FilePath -> FilePath -> IO BS.ByteString
serveFile docRoot path = do
    let fullPath = docRoot </> dropWhile (== '/') path
    exists <- doesFileExist fullPath
    if exists
        then do
            content <- BS.readFile fullPath
            let mimeType = getMimeType (takeExtension fullPath)
                headers = [("Content-Type", mimeType), 
                          ("Content-Length", show $ BS.length content)]
            return $ buildResponse 200 "OK" headers content
        else return $ buildResponse 404 "Not Found" [("Content-Type", "text/plain")] "File not found"

-- Directory listing
listDirectory' :: FilePath -> IO BS.ByteString
listDirectory' path = do
    exists <- doesFileExist path
    if exists
        then do
            files <- listDirectory path
            let fileList = unlines $ map ("- " ++) files
                html = "<html><body><h1>Directory Listing</h1><pre>" ++ fileList ++ "</pre></body></html>"
                headers = [("Content-Type", "text/html")]
            return $ buildResponse 200 "OK" headers (C.pack html)
        else return $ buildResponse 404 "Not Found" [("Content-Type", "text/plain")] "Directory not found"

-- MIME type detection
getMimeType :: String -> String
getMimeType ext = case ext of
    ".html" -> "text/html"
    ".htm"  -> "text/html"
    ".css"  -> "text/css"
    ".js"   -> "application/javascript"
    ".json" -> "application/json"
    ".xml"  -> "application/xml"
    ".txt"  -> "text/plain"
    ".jpg"  -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".png"  -> "image/png"
    ".gif"  -> "image/gif"
    ".pdf"  -> "application/pdf"
    ".zip"  -> "application/zip"
    ".mp4"  -> "video/mp4"
    ".mp3"  -> "audio/mpeg"
    _       -> "application/octet-stream"

-- Session management
generateSessionId :: IO String
generateSessionId = do
    time <- getCurrentTime
    return $ show $ hash $ C.pack $ show time

createSession :: SessionStore -> String -> IO String
createSession store clientInfo = do
    sessionId <- generateSessionId
    now <- getCurrentTime
    let session = ClientSession sessionId False now clientInfo
    atomically $ modifyTVar store (Map.insert sessionId session)
    return sessionId

getSession :: SessionStore -> String -> IO (Maybe ClientSession)
getSession store sessionId = do
    sessions <- readTVarIO store
    return $ Map.lookup sessionId sessions

-- Authentication (basic)
authenticateUser :: String -> String -> Bool
authenticateUser username password = 
    -- Simple hardcoded auth - replace with proper auth system
    username == "admin" && password == "password"

-- API endpoints
handleApiRequest :: SessionStore -> LogStore -> String -> [(String, String)] -> BS.ByteString -> IO BS.ByteString
handleApiRequest sessionStore logStore path headers body = case path of
    "/api/status" -> return $ buildResponse 200 "OK" 
        [("Content-Type", "application/json")] 
        "{\"status\":\"running\",\"version\":\"1.0\"}"
    
    "/api/sessions" -> do
        sessions <- readTVarIO sessionStore
        let sessionsList = Map.elems sessions
        return $ buildResponse 200 "OK" 
            [("Content-Type", "application/json")] 
            (L.toStrict $ encode sessionsList)
    
    "/api/logs" -> do
        logs <- readTVarIO logStore
        return $ buildResponse 200 "OK" 
            [("Content-Type", "application/json")] 
            (L.toStrict $ encode $ take 100 logs) -- Return last 100 entries
    
    "/api/system" -> do
        uptime <- readProcess "uptime" [] ""
        let response = object ["uptime" .= uptime]
        return $ buildResponse 200 "OK" 
            [("Content-Type", "application/json")] 
            (L.toStrict $ encode response)
    
    "/api/upload" -> do
        -- Simple file upload handler
        let filename = fromMaybe "upload.dat" (lookup "filename" headers)
        BS.writeFile ("uploads/" ++ filename) body
        return $ buildResponse 200 "OK" 
            [("Content-Type", "application/json")] 
            "{\"message\":\"File uploaded successfully\"}"
    
    _ -> return $ buildResponse 404 "Not Found" 
        [("Content-Type", "application/json")] 
        "{\"error\":\"API endpoint not found\"}"

-- WebSocket-like long polling
handleWebSocket :: Context -> SessionStore -> IO ()
handleWebSocket ctx sessionStore = do
    -- Simple echo service with session awareness
    forever $ do
        msg <- recvData ctx
        let msgStr = C.unpack msg
        case parseCommand msgStr of
            ("PING", _) -> sendData ctx "PONG"
            ("ECHO", content) -> sendData ctx ("ECHO: " <> C.pack content)
            ("TIME", _) -> do
                now <- getCurrentTime
                sendData ctx (C.pack $ "TIME: " ++ show now)
            ("SESSIONS", _) -> do
                sessions <- readTVarIO sessionStore
                let count = Map.size sessions
                sendData ctx (C.pack $ "ACTIVE_SESSIONS: " ++ show count)
            _ -> sendData ctx "UNKNOWN_COMMAND"
  where
    parseCommand str = 
        let (cmd, rest) = break (== ' ') str
        in (cmd, drop 1 rest)

-- Logging functionality
logRequest :: LogStore -> String -> String -> String -> Int -> String -> IO ()
logRequest logStore clientAddr method path status message = do
    now <- getCurrentTime
    let entry = LogEntry now clientAddr method path status message
    atomically $ modifyTVar logStore (entry :)

-- Main request handler
handleClient :: Context -> SessionStore -> LogStore -> ServerConfig -> String -> IO ()
handleClient ctx sessionStore logStore config clientAddr = do
    result <- catch (do
        request <- recvData ctx
        let requestStr = C.unpack request
        
        case parseRequest request of
            Nothing -> do
                let response = buildResponse 400 "Bad Request" [("Content-Type", "text/plain")] "Invalid request format"
                sendData ctx response
                logRequest logStore clientAddr "INVALID" "/" 400 "Bad request format"
            
            Just (method, path, headers, body) -> do
                logRequest logStore clientAddr method path 200 "Processing"
                
                response <- case (method, path) of
                    ("GET", "/") -> serveFile (documentRoot config) "index.html"
                    ("GET", p) | "/api/" `isPrefixOf` p -> handleApiRequest sessionStore logStore p headers body
                    ("POST", p) | "/api/" `isPrefixOf` p -> handleApiRequest sessionStore logStore p headers body
                    ("GET", "/ws") -> do
                        sendData ctx "HTTP/1.1 101 Switching Protocols\r\n\r\n"
                        handleWebSocket ctx sessionStore
                        return ""
                    ("GET", "/dir") -> listDirectory' (documentRoot config)
                    ("GET", p) -> serveFile (documentRoot config) p
                    ("POST", "/upload") -> do
                        createDirectoryIfMissing True "uploads"
                        let filename = fromMaybe "upload.dat" (lookup "filename" headers)
                        BS.writeFile ("uploads/" ++ filename) body
                        return $ buildResponse 200 "OK" [("Content-Type", "text/plain")] "File uploaded"
                    _ -> return $ buildResponse 405 "Method Not Allowed" [("Content-Type", "text/plain")] "Method not allowed"
                
                when (not $ BS.null response) $ sendData ctx response
        
        bye ctx
        ) (\e -> do
            hPutStrLn stderr $ "Error handling client: " ++ show (e :: SomeException)
            logRequest logStore clientAddr "ERROR" "/" 500 (show e)
        )
    return ()

-- Server initialization
initializeServer :: ServerConfig -> IO (SessionStore, LogStore)
initializeServer config = do
    -- Create necessary directories
    createDirectoryIfMissing True (documentRoot config)
    createDirectoryIfMissing True "uploads"
    createDirectoryIfMissing True "logs"
    
    -- Create default index.html if it doesn't exist
    let indexPath = documentRoot config </> "index.html"
    indexExists <- doesFileExist indexPath
    unless indexExists $ do
        let defaultIndex = "<html><body><h1>Haskell TLS Server</h1><p>Server is running!</p><a href=\"/api/status\">API Status</a></body></html>"
        writeFile indexPath defaultIndex
    
    -- Initialize stores
    sessionStore <- newTVarIO Map.empty
    logStore <- newTVarIO []
    
    return (sessionStore, logStore)

-- Main server function
main :: IO ()
main = do
    let config = ServerConfig 
            { port = "8443"
            , certFile = "server.crt" 
            , keyFile = "server.key"
            , documentRoot = "www"
            , logFile = "logs/server.log"
            , maxConnections = 100
            , enableAuth = False
            }
    
    putStrLn "Initializing Comprehensive TLS Server..."
    (sessionStore, logStore) <- initializeServer config
    
    -- Load TLS credentials
    cert <- credentialLoadX509 (certFile config) (keyFile config)
    creds <- either error return cert
    
    let tlsParams = def
            { serverShared = def { sharedCredentials = Credentials [creds] }
            , serverSupported = def { supportedCiphers = ciphersuite_default }
            }
    
    -- Setup socket
    addrinfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                              Nothing (Just $ port config)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock (maxConnections config)
    
    putStrLn $ "Comprehensive TLS Server running on port " ++ port config
    putStrLn $ "Document root: " ++ documentRoot config
    putStrLn $ "Features enabled:"
    putStrLn $ "  - HTTPS/TLS encryption"
    putStrLn $ "  - File serving"
    putStrLn $ "  - RESTful API endpoints"
    putStrLn $ "  - File upload/download"
    putStrLn $ "  - Session management" 
    putStrLn $ "  - Request logging"
    putStrLn $ "  - WebSocket-like functionality"
    putStrLn $ "  - Directory listing"
    putStrLn $ "  - MIME type detection"
    putStrLn "Available endpoints:"
    putStrLn "  GET  /              - Main page"
    putStrLn "  GET  /api/status    - Server status"
    putStrLn "  GET  /api/sessions  - Active sessions"
    putStrLn "  GET  /api/logs      - Request logs"
    putStrLn "  GET  /api/system    - System information"
    putStrLn "  POST /api/upload    - File upload"
    putStrLn "  GET  /ws            - WebSocket-like connection"
    putStrLn "  GET  /dir           - Directory listing"
    
    forever $ do
        (conn, addr) <- accept sock
        let clientAddr = show addr
        putStrLn $ "Connection from: " ++ clientAddr
        
        -- Handle each client in a separate thread
        void $ forkIO $ bracket
            (contextNew conn tlsParams)
            (\ctx -> catch (bye ctx) (\_ -> return () :: IO ()))
            (\ctx -> do
                handshake ctx
                handleClient ctx sessionStore logStore config clientAddr
            )
