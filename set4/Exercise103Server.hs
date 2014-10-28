import Network.Socket
import Network.BSD
import Control.Concurrent
import Control.Concurrent.STM

emptyClientList :: STM (TVar [(Socket, SockAddr)])
emptyClientList = newTVar []

appendClient :: TVar [(Socket, SockAddr)] -> (Socket, SockAddr) -> STM ()
appendClient clientList connection = do
	currentList <- readTVar clientList
	writeTVar clientList (connection:currentList)

removeClient :: TVar [(Socket, SockAddr)] -> (Socket, SockAddr) -> STM ()
removeClient clientList connection = do
	currentList <- readTVar clientList
	writeTVar clientList (filter (/= connection) currentList)

broadcastMessage :: TVar [(Socket, SockAddr)] -> String -> IO ()
broadcastMessage clientList message = do
	currentList <- atomically $ readTVar clientList
	mapM (((flip send) message) . fst) currentList
	return ()

processConnection :: Socket -> TVar [(Socket, SockAddr)] -> IO ()
processConnection serverSocket clientList = do
	(clientSocket, clientAddr) <- accept serverSocket

	atomically $ appendClient clientList (clientSocket, clientAddr)

	processMessage clientList (clientSocket, clientAddr)

processMessage :: TVar [(Socket, SockAddr)] -> (Socket, SockAddr) -> IO () 
processMessage clientList connection@(clientSocket, clientAddr) = do
	message <- recv clientSocket 1024

	if length message == 0 then
		atomically $ removeClient clientList connection
	else
		do
			--broadcastMessage clientList message
			print message
			processMessage clientList connection

main = withSocketsDo $ do
	proto <- getProtocolNumber "tcp"
	serverSocket <- socket AF_INET Stream proto
	
	clientList <- atomically emptyClientList

	addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just "8080") 
	let addr = head addrsInfo

	bind serverSocket (addrAddress addr)
	listen serverSocket 5

	forkIO $ processConnection serverSocket clientList

	close serverSocket

	print "exit"