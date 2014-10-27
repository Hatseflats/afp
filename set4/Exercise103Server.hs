import Network.Socket
import Network.BSD
import Control.Concurrent.STM

processConnection :: Socket -> IO ()
processConnection s = do
	(clientSocket, clientAddress) <- accept s
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
	s <- socket AF_INET Stream proto

	addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just "8080") 
	let addr = head addrsInfo

	bind s (addrAddress addr)
	listen s 5

	processConnection s

	close s

	print "exit"