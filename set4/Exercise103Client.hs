import Network.Socket
import Network.BSD

import Control.Concurrent.STM
import Control.Concurrent

import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Port String | Nickname String
    deriving Show

options :: [OptDescr Flag]
options =
    [ 
        Option ['p'] ["port"] (ReqArg Port "string") "portnumber",
        Option ['n'] ["nickname"] (ReqArg Nickname "string") "nickname"
    ] 

main = do 
    args <- getArgs
    case getOpt RequireOrder options args of
        (o,_,[]) -> makeConnection (getNickname o) (getPort o)
        (_,_,e) -> print e

getPort :: [Flag] -> String
getPort flags = 
   head $ [p | Port p <- flags]

getNickname :: [Flag] -> String
getNickname flags = 
    head $ [n | Nickname n <- flags]

makeConnection :: String -> String -> IO ()
makeConnection nickname portnumber = withSocketsDo $ do 
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto

  addrsInfo <- getAddrInfo Nothing (Just "localhost") (Just portnumber) 
  let addr = head addrsInfo

  connect sock (addrAddress addr)

  forkIO $ receiveLoop sock

  sendNickname sock nickname
  messageLoop sock

  print "done!"

messageLoop :: Socket -> IO ()
messageLoop sock = do
    message <- getLine
    send sock message
    messageLoop sock

sendNickname :: Socket -> String -> IO Int
sendNickname sock nickname = do
    send sock nickname

receiveLoop :: Socket -> IO ()
receiveLoop sock = do
    message <- recv sock 1024
    print message
    receiveLoop sock

