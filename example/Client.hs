{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
 
import           Control.Natural
import           Control.Remote.Monad.Binary

import Network.Socket as Sock hiding (send)
import qualified Network.Socket.ByteString.Lazy as NBS
import  System.Environment

import Types

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

clientSend :: Socket -> IO (SendAPI :~> IO)
clientSend sock = return$ Nat ( \ (Sync bs) ->
             do
               print bs
               NBS.send sock bs
               putStrLn "Sent"
               res <- NBS.recv sock 4096
               putStrLn $ "Received: " ++ (show res)
               return res
              )

main::IO()
main = do
        port <- getArgs
        sock <- createSocket "localhost" $ head port
        (Nat f) <- clientSend sock
        let s = monadClient f       
        res <- send s $ do push 9; pop
        print res
        res2 <- send s $ do add <$> pop <*> pure 3
        print res2

        return ()
