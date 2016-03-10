{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
 
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary
import           Control.Remote.Monad (Promote(..))

import qualified Data.ByteString.Lazy as BS
import Network.Socket as Sock hiding (send)
import qualified Network.Socket.ByteString.Lazy as NBS
import           System.Environment
import Types

import           Control.Monad.State
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM

--User function simulating a remote stack. 
dispatchWeakPacket :: (TMVar [Int])-> WP.WeakPacket Command Procedure a -> IO a
dispatchWeakPacket var (WP.Command c@(Push n)) = do 
                                       putStrLn $ "Push "++ (show n)
                                       st <- atomically $ takeTMVar var
                                       let (a,s) = runState (evalCommand c) st
                                       print s
                                       atomically $ putTMVar var s
                                       return a
dispatchWeakPacket var (WP.Procedure p) = do 
                                         putStrLn $ "Pop"
                                         st <- atomically $ takeTMVar var
                                         let (a,s) = runState (evalProcedure p) st
                                         print s
                                         atomically $ putTMVar var s
                                         return a


evalProcedure :: Procedure a-> State [Int] a 
evalProcedure (Pop) = do st <- get
                         case st of
                           []  -> error "Can't pop an empty stack"
                           (x:xs) -> do
                                      put xs
                                      return x 

evalCommand :: Command -> State [Int] ()
evalCommand (Push n) = modify (n:)


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
                        -- End of Message  (implementation detail with Sockets)
                        "" -> return ()

                        _ -> do 
                                res <- f1 (Sync bs) 
                                let res' = BS.append  "FF" res
                                NBS.sendAll sock $ res'
                                loop sock (Nat f1)


main::IO()
main = do
        port <- getArgs
        case port of
          [] -> error "ERROR: Requires Port number as argument"
          _  -> socketServer $ head  port
