import Network.Socket
import Network.BSD
import Control.Concurrent.STM

main = withSocketsDo $ do 
	proto <- getProtocolNumber "tcp"
	s <- socket AF_INET Stream proto

	addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just "8080") 
	let addr = head addrsInfo

	connect s (addrAddress addr)

	sendstuff s

	print "done!"

sendstuff :: Socket -> IO ()
sendstuff sock = do
	message <- getLine
	send sock message
	sendstuff sock