module Main where
import System.Exit
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Options.Applicative
import Control.Concurrent
import Control.Exception
import Data.List.Split

main = do
    putStrLn "Server Port:"
    port <- getLine
    s <- createConnection (read port)
    connLoop s

createConnection:: Int -> IO Socket
createConnection p = do 
    s <- socket AF_INET Stream 0
    setSocketOption s ReuseAddr 1
    bind s (SockAddrInet (read $ show p) iNADDR_ANY)
    listen s 10
    return s

connLoop :: Socket -> IO ()
connLoop s = do
    putStrLn "in connLoop"
    conn <- accept s
    id <- myThreadId
    forkIO $ handleConn conn id
    connLoop s

handleConn :: (Socket, SockAddr) -> ThreadId -> IO ()
handleConn (s, _) id = do
    putStrLn "handling conn" 
    input <- recv s 2048
    parseMessage s id input


parseMessage :: Socket -> ThreadId -> String -> IO ()
parseMessage s id "GET /echo.php?message=kill_service HTTP/1.1\r\n\r\n" = do
    putStrLn "HELO sent"
    send s "qwertyuiop"
    throwTo id ThreadKilled
    close s
parseMessage s _ "GET /echo.php?message=helo_text HTTP/1.1\r\n\r\n" = do
    name <- getSocketName s
    putStrLn ((splitOn ":" (show name))!!0)
    putStrLn ((splitOn ":" (show name))!!1)
    putStrLn "KILL sent"
    send s "sevice killed"
    close s
parseMessage s _ m = do
    putStrLn $ m ++ " sent"
    send s "message recieved"
    close s
