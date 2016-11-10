module Main where
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as B8
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Options.Applicative
import Control.Concurrent
import Control.Exception
import Data.List.Split

main = do
    port:_ <- getArgs
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
    conn <- accept s
    id <- myThreadId
    forkIO $ handleConn conn id
    connLoop s

handleConn :: (Socket, SockAddr) -> ThreadId -> IO ()
handleConn (s, _) id = do 
    input <- recv s 4096
    parseMessage s id (show input)

parseMessage :: Socket -> ThreadId -> String -> IO ()
parseMessage s id ('"':'K':'I':'L':'L':'_':'S':'E':'R':'V':'I':'C':'E':_) = do
    putStrLn "Shutting Down"
    throwTo id ThreadKilled
    close s
parseMessage s _  ('"':'H':'E':'L':'O':m) = do
    name <- getSocketName s
    send s  (B8.pack $ ("HELO" ++ (take ((length m) -1)  m) ++ "IP:" ++ ((splitOn ":" (show name))!!0) ++ "\nPort:"++ ((splitOn ":" (show name))!!1) ++ "\nStudentID:13327420\n"))
    putStrLn "Helo text sent"
    close s
parseMessage s _ m = do
    putStrLn ("some other message recieved(" ++ m ++ ")")
    close s
