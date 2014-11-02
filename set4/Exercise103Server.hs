module Server where

import Prelude hiding (catch)

import Network.Socket
import Network.BSD

import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM

emptyClientList :: STM (TVar [(String, Socket)])
emptyClientList = newTVar []

appendClient :: TVar [(String, Socket)] -> (String, Socket) -> STM ()
appendClient clientList connection = do
	currentList <- readTVar clientList
	writeTVar clientList (connection:currentList)

removeClient :: TVar [(String, Socket)] -> (String, Socket) -> STM ()
removeClient clientList connection = do
	currentList <- readTVar clientList
	writeTVar clientList (filter (/= connection) currentList)

broadcastMessage :: TVar [(String, Socket)] -> String -> IO [Int]
broadcastMessage clientList message = do
	currentList <- atomically $ readTVar clientList
	mapM (\(nickname, clientSocket) -> send clientSocket message) currentList

processConnection :: Socket -> TVar [(String, Socket)] -> IO ()
processConnection serverSocket clientList = do
	(clientSocket, _) <- accept serverSocket

	nickname <- recv clientSocket 1024

	let connection = (nickname, clientSocket)

	broadcastMessage clientList (nickname ++ " has joined the chat")
	atomically $ appendClient clientList connection
	forkIO $ processMessage clientList connection `catch` disconnect clientList connection

	processConnection serverSocket clientList

disconnect :: TVar [(String, Socket)] -> (String, Socket) ->  SomeException -> IO ()
disconnect clientList connection@(nickname, clientSocket) exception = 
	do
		close clientSocket
		atomically $ removeClient clientList connection
		broadcastMessage clientList (nickname ++ "has left the chat")
		return ()

processMessage :: TVar [(String, Socket)] -> (String, Socket) -> IO () 
processMessage clientList connection@(nickname, clientSocket) = do
	message <- recv clientSocket 1024

	ints <- broadcastMessage clientList (nickname ++ " :" ++ message)
	processMessage clientList connection
	

main = withSocketsDo $ do
	proto <- getProtocolNumber "tcp"
	serverSocket <- socket AF_INET Stream proto
	
	clientList <- atomically emptyClientList

	addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just "8080") 
	let addr = head addrsInfo

	bind serverSocket (addrAddress addr)
	listen serverSocket 5

	processConnection serverSocket clientList

	close serverSocket

	print "exit"