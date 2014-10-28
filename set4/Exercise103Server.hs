import Network.Socket
import Network.BSD
import Control.Concurrent.STM

emptyClientList :: IO (TVar [(Socket, SockAddr)])
emptyClientList = newTVarIO []

appendClient :: TVar [(Socket, SockAddr)] -> (Socket, SockAddr) -> STM ()
appendClient clientList connection = do
	currentList <- readTVar clientList
	writeTVar clientList (connection:currentList)

processConnection :: Socket ->  TVar [(Socket, SockAddr)] -> IO ()
processConnection serverSocket clientList = do
	(clientSocket, clientAddress) <- accept serverSocket
	processMessage clientSocket

processMessage :: Socket -> IO () 
processMessage clientSocket = do
	message <- recv clientSocket 1024

	if length message == 0 then
		print "disconnect"
	else
		do
			print message
			processMessage clientSocket

main = withSocketsDo $ do
	proto <- getProtocolNumber "tcp"
	serverSocket <- socket AF_INET Stream proto
	clientList <- emptyClientList

	addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just "8080") 
	let addr = head addrsInfo

	bind serverSocket (addrAddress addr)
	listen serverSocket 5

	--processConnection serverSocket

	close serverSocket

	print "exit"