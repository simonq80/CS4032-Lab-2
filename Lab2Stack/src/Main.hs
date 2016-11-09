module Main where
import System.Exit
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Options.Applicative
import Control.Concurrent

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
    forkIO $ handleConn conn myThreadId
    connLoop s

handleConn :: (Socket, SockAddr) -> IO ThreadId -> IO ()
handleConn (s, _) _ = do
    putStrLn "handling conn" 
    input <- recv s 2048
    parseMessage s input


parseMessage :: Socket -> String -> IO ()
parseMessage s "GET /echo.php?message=asdf HTTP/1.1\r\n\r\n" = do
    putStrLn "HELO sent"
    send s "qwertyuiop"
    error "server exit"
    close s
parseMessage s "KILL_SERVICE\n" = do
    putStrLn "KILL sent"
    send s "sevice killed"
    close s
parseMessage s m = do
    putStrLn $ m ++ " sent"
    send s "message recieved"
    close s
