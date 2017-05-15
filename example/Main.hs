{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import           Control.Natural
import           Control.Remote.Monad           (RemoteMonad)
import           Control.Remote.Monad.Binary
import           Control.Remote.Packet          (promote)
import qualified Control.Remote.Packet.Weak     as WP
import qualified Data.ByteString.Lazy           as BS
import           Network.Socket                 as Sock hiding (send)
import qualified Network.Socket.ByteString.Lazy as NBS
import           System.Environment
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State

import           Types
-- =========== Server Code ==============
--User function simulating a remote stack.
dispatchWeakPacket :: (TMVar [Int])-> WP.WeakPacket Prim a -> IO a
dispatchWeakPacket var (WP.Primitive p) = do
                                             () <- case p of
                                                     Push n -> putStrLn $ "Push "++ (show n)
                                                     Pop    -> putStrLn "Pop"
                                             st <- atomically $ takeTMVar var
                                             let (a,s) = runState (evalPrim p) st
                                             print s
                                             atomically $ putTMVar var s
                                             return a
evalPrim :: Prim a -> State [Int] a
evalPrim (Push n) = modify (n:)
evalPrim (Pop) = do
                    st <- get
                    case st of
                      []  -> error "Can't pop an empty stack"
                      (x:xs) -> do
                                   put xs
                                   return x

--
--Lift user function into a natural transformation
runWeakBinary ::  TMVar [Int] -> WP.WeakPacket Prim :~> IO
runWeakBinary var =  wrapNT (dispatchWeakPacket var)

socketServer ::String -> IO()
socketServer port = do
                       addrinfo <- getAddrInfo (Just (defaultHints {addrFlags=[AI_PASSIVE]})) Nothing (Just port)
                       let serveraddr = head addrinfo
                       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                       bind sock (addrAddress serveraddr)
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
         loop sock (NT f1) = do
                     bs <- NBS.recv sock 4096
                     case bs of
                        -- End of Message  (implementation detail with Sockets)
                        "" -> return ()

                        _ -> do
                                res <- f1 (Sync bs)
                             --   let res' = BS.append  "FF" res
                                NBS.sendAll sock $ res
                                loop sock (NT f1)


-- ============= Client Code =============
add :: Int -> Int -> Int
add x y = x +  y

createSocket :: HostName -> String -> IO Socket
createSocket host port = do
          addrinfo <- getAddrInfo Nothing (Just host) (Just port)
          let serveraddr = head addrinfo
          sock <- socket (addrFamily serveraddr) (addrSocketType serveraddr) (addrProtocol serveraddr)
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress serveraddr)
          return sock

--Networking mechanism to send ByteString across a socket
clientSend :: Socket -> IO (SendAPI :~> IO )
clientSend sock = return$ NT ( \ (Sync bs) ->
             do
               case bs of

                  "\ETX" -> return $ BS.pack []
                  _ ->do
                     NBS.sendAll sock bs
                     res <- NBS.recv sock 4096
                     return res
              )
-- Initializes socket and setup as sending method for RemoteMonad
createSession :: String -> String -> IO (RemoteMonad Prim :~> IO)
createSession host port =do
               sock <- createSocket host port
               (NT f) <- clientSend sock
               return $ monadClient f

createSession2 :: IO (RemoteMonad Prim :~> IO)
createSession2 = do
                   var <- newTMVarIO []
                   return $ monadClient (unwrapNT $ server $ promote (runWeakBinary var))

example :: (RemoteMonad Prim :~> IO) -> IO()
example s = do
        putStrLn " push 2; push 1;"
        send s $ do
                   push 2
                   push 1

        res <- send s $ do
                   push 9
                   pop
        putStrLn "push 9; pop:"
        print res
        res2 <- send s $ do add <$> pop <*> (pure 3)
        putStrLn "add <$> pop <*> pure 3:"
        print res2

        res3 <- send s $ do
                            push 3
                            r1 <- pop
                            r2 <- pop
                            push 5
                            return (r1,r2)
        putStrLn "push 3; r1<- pop; push 4; r2<- pop; push5; return (r1,r2):"
        print res3


main :: IO()
main = getArgs >>= main2

main2:: [String] -> IO()
main2 []  = do
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering
--   _ <- forkIO $ main2 ["server"]
   threadDelay $ 1000 * 1000
   main2 ["noSocketClient"]

main2 ("noSocketClient":_) = do
        s <- createSession2
        example s
main2 ("client":_)= do
        let port ="5500"
        s <-createSession "localhost" port
        example s
main2 ("server":_) = do
        let port ="5500"
        case port of
          [] -> error "ERROR: Requires Port number as argument"
          _  -> socketServer  port
main2 _ = error "Unkown main option, run with client or with server"
