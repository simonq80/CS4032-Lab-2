module Main where
import System.IO
import qualified Data.ByteString.Char8 as B8
import Network.Socket
import Options.Applicative
import Control.Parallel

main = do
	putStrLn "Server Port:"
	port <- getLine
	s <- createConnection ip (read port)
	m <- sendMess s message
	putStrLn "Returned:"
	B8.putStrLn m

createConnection:: Int -> IO Socket
createConnection p = do 
	s <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
`	bind sock (SockAddrInet p iNADDR_ANY)
	listen s 10
	return s

connLoop :: Socket -> IO ()
connLoop s = do
	conn <- accept s
	par (handleConn conn) (connLoop s)

handleConn :: (Socket, SockAddr) -> IO ()
	
