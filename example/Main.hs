{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary
import           Control.Remote.Monad.Packet (promote)
import           Control.Remote.Monad
import           Control.Natural
import qualified Data.ByteString.Lazy as BS
import Network.Socket as Sock hiding (send)
import qualified Network.Socket.ByteString.Lazy as NBS
import           System.Environment
import           System.IO

import           Control.Monad.State
import           Control.Concurrent
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM
import           Control.Exception

import           Types
-- =========== Server Code ==============
--User function simulating a remote stack. 
dispatchWeakPacket :: (TMVar [Int])-> WP.WeakPacket Command Procedure a -> IO a
dispatchWeakPacket var (WP.Command c@(Push n)) = do 
                                       putStrLn $ "Push "++ (show n)
                                       st <- atomically $ takeTMVar var
                                       let (a,s) = runState (evalCommand c) st
                                       print s
                                       atomically $ putTMVar var s
                                       case a of
                                         Left s -> error s
                                         Right a' -> return a'
dispatchWeakPacket var (WP.Procedure p) = do 
                                         putStrLn $ "Pop"
                                         st <- atomically $ takeTMVar var
                                         let (a,s) = runState (evalProcedure p) st
                                         print s
                                         atomically $ putTMVar var s
                                         case a of
                                           Left s -> error s
                                           Right a' -> return a'


evalProcedure :: Procedure a-> State [Int] (Either String a) 
evalProcedure (Pop) = do st <- get
                         case st of
                           []  ->  return $ Left "Can't pop an empty stack" --return $ Left "Can't pop an empty stack"
                           (x:xs) -> do
                                      put xs
                                      return $ Right x 
evalProcedure _p = return $ Left "Unrecognized procedure"

evalCommand :: Command -> State [Int] (Either String ())
evalCommand (Push n) =  do modify (n:)
                           return $ Right ()
                         
evalCommand _c = return $ Left "Unrecognized command"

--
--Lift user function into a natural transformation
runWeakBinary ::  TMVar [Int] -> WP.WeakPacket Command Procedure :~> IO
runWeakBinary var =  nat (dispatchWeakPacket var)

socketServer ::String -> IO()
socketServer port = do 
                       addrinfo <- getAddrInfo (Just (defaultHints {addrFlags=[AI_PASSIVE]})) Nothing (Just port)
                       let serveraddr = head addrinfo
                       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                       bindSocket sock (addrAddress serveraddr)
                       putStrLn $ "Listening on " ++ (show port)
                       listen sock 2
                       var <- newTMVarIO []
                       sockHandler sock $ server $ promote (runWeakBinary var)

sockHandler :: Socket -> (SendAPI :~> IO) -> IO ()
sockHandler s f = do 
           (sock, addr) <- accept s
           putStrLn $ "Accepted socket from : "++ (show addr)
           loop sock f
           return ()
       where
        -- Receive bytestring from client and frame response with "FF"
         loop :: Socket -> (SendAPI :~> IO) -> IO()
         loop sock (Nat f1) = do
                     bs <- NBS.recv sock 4096
                     case bs of 
                       "" -> return ()
                       _  -> do
                         res <- f1 (Sync bs) 
                         NBS.sendAll sock $ res
                         loop sock (Nat f1)


-- ============= Client Code =============
add :: Int -> Int -> Int
add x y = x +  y

createSocket :: HostName -> String -> IO Socket
createSocket host port = do
          addrinfo <- getAddrInfo Nothing (Just host) (Just port)
          let serveraddr = head addrinfo
          sock <- socket (addrFamily serveraddr) Stream defaultProtocol
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress serveraddr)
          return sock

--Networking mechanism to send ByteString across a socket
clientSend :: Socket -> IO (SendAPI :~> IO )
clientSend sock = return$ Nat ( \ (Sync bs) ->
             do
             -- NBS.sendAll sock bs -- correct one
              NBS.sendAll sock (BS.append (BS.pack [8,9,10]) bs)
              res <- NBS.recv sock 4096
              return res
              )
-- Initializes socket and setup as sending method for RemoteMonad 
createSession :: String -> String -> IO (RemoteMonad Command Procedure :~> IO)
createSession host port =do 
               sock <- createSocket host port 
               (Nat f) <- clientSend sock
               return $ monadClient f
--
-- Session without using Sockets
createSession2 :: IO (RemoteMonad Command Procedure :~> IO)
createSession2 = do
                   var <- newTMVarIO []
                   return $ monadClient (run $ server $ promote (runWeakBinary var))

main :: IO()
main = getArgs >>= main2

main2:: [String] -> IO()
main2 []  = do
   hSetBuffering stdout LineBuffering 
   hSetBuffering stderr LineBuffering 
   _ <- forkIO $ main2 ["server"]          
   threadDelay $ 1000 * 1000          
   main2 ["client"]         

main2 ("client":_)= do
        let port ="5500"
        case port of
              [] -> error "ERROR: Requires port number as argument"
              _  -> do
                      sock <- createSocket "129.237.120.39" port
                      (Nat f) <- clientSend sock
                      let s =  monadClient f

                      putStrLn "push 2\npush 1"
                      send s $ do
                                 push 2
                                 push 1

                      res <- send s $ do 
                                 push 9
                                 pop
                      putStrLn "push 9\npop:"
                      print res
                      res2 <- send s $ do add <$> pop <*> (pure 3)
                      putStrLn "add <$> pop <*> pure 3:"
                      print res2
                      
                      res3 <- send s $ do push 3
                                          r1 <- pop
                                          push 4
                                          r2 <- pop
                                          push 5
                                          return (r1,r2)
                      putStrLn "push 3\nr1<- pop\npush 4\nr2<- pop\npush5\nreturn (r1,r2):" 
                      print res3
                      close sock


main2 ("server":_) = do
        let port ="5500"
        socketServer  port

