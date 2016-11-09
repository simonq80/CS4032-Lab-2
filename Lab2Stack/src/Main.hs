module Main where
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Options.Applicative
import Control.Parallel

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
	handleConn conn
	connLoop s

handleConn :: (Socket, SockAddr) -> IO ()
handleConn (s, _) = do
    putStrLn "handling conn"
    send s "Hello!\n"
    close s
	
