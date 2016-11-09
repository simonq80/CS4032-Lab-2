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
parseMessage s id "KILL_SERVICE\n" = do
    putStrLn "Shutting Down"
    throwTo id ThreadKilled
    close s
parseMessage s _ "HELO text\n" = do
    name <- getSocketName s
    send s  ("HELO text\nIP:" ++ ((splitOn ":" (show name))!!0) ++ "\nPort:"++ ((splitOn ":" (show name))!!1) ++ "\nStudentID:13327420\n")
    putStrLn "Helo text sent"
    close s
parseMessage s _ m = do
    putStrLn "some other message recieved"
    close s
