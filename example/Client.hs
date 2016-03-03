{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

 
import           Control.Natural
import           Control.Remote.Monad.Binary
import           Control.Remote.Monad

import           Network.Socket as Sock hiding (send)
import qualified Network.Socket.ByteString.Lazy as NBS
import qualified Data.ByteString.Lazy as BS (pack,drop)
import           System.Environment

import           Types

add :: Int -> Int -> Int
add x y = x + y

createSocket :: HostName -> String -> IO Socket
createSocket host port = do
          addrinfo <- getAddrInfo Nothing (Just host) (Just port)
          let serveraddr = head addrinfo
          sock <- socket (addrFamily serveraddr) Stream defaultProtocol
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress serveraddr)
          return sock

--Networking mechanism to send ByteString across a socket
clientSend :: Socket -> IO (SendAPI :~> IO)
clientSend sock = return$ Nat ( \ (Sync bs) ->
             do
               case bs of

                  "\ETX" -> return $ BS.pack []
                  _ ->do
                     NBS.sendAll sock bs
                     res <- NBS.recv sock 4096

                     --Everything from server has FF at front
                     --(in order to send () (empty string)
                     return $ BS.drop 2 res
              )
-- Initializes socket and setup as sending method for RemoteMonad 
createSession :: String -> String -> IO (RemoteMonad Command Procedure :~> IO)
createSession host port =do 
               sock <- createSocket host port 
               (Nat f) <- clientSend sock
               return $ monadClient f
main::IO()
main = do
        port <- getArgs
        case port of
              [] -> error "ERROR: Requires port number as argument"
              _  -> do
                      s <-createSession "localhost" $ head port       
                      res <- send s $ do 
                                 push 9
                                 pop
                      putStrLn "push 9; pop:"
                      print res
                      res2 <- send s $ do add <$> pop <*> pure 3
                      putStrLn "add <$> pop <*> pure 3:"
                      print res2
                      
                      res3 <- send s $ do push 3
                                          r1 <- pop
                                          push 4
                                          r2 <- pop
                                          push 5
                                          return (r1,r2)
                      putStrLn "push 3; r1<- pop; push 4; r2<- pop; push5; return (r1,r2):" 
                      print res3
