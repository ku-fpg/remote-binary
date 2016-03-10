{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
 
import           Control.Natural
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary


import           Control.Remote.Monad.Transport
import           Network.Transport  hiding (send)
import           Network.Transport.TCP
import           System.Environment (getArgs)
import           Control.Concurrent  (forkIO, threadDelay)
import           System.IO
import           Control.Monad.State
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM
import qualified Data.Binary as B

data Command :: * where
   Push :: Int -> Command   -- PUSH
 deriving (Show)

instance B.Binary Command where
   put (Push n) = B.put n
   get = do i <- B.get
            return $ Push i

data Procedure :: * -> * where
  Pop :: Procedure Int    -- POP

instance BinaryQ Procedure where
   getQ = do i <- B.get
             case i :: B.Word8 of
               0 -> return $ Fmap B.put Pop
   putQ (Pop)= B.put (0 :: B.Word8) 

   interpQ (Pop) = B.get



push :: Int -> RemoteMonad Command Procedure ()
push n = command $ Push n

pop :: RemoteMonad Command Procedure Int
pop = procedure $ Pop


dispatchWeakPacket :: TMVar [Int] -> WP.WeakPacket Command Procedure a -> IO a
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

runWeakBinary ::  TMVar [Int] -> WP.WeakPacket Command Procedure :~> IO
runWeakBinary var =  nat (dispatchWeakPacket var)

evalProcedure :: Procedure a-> State [Int] a 
evalProcedure (Pop) = do st <- get
                         case st of
                           []  -> error "Can't pop an empty stack"
                           (x:xs) -> do
                                      put xs
                                      return x 

evalCommand :: Command -> State [Int] ()
evalCommand (Push n) = modify (n:)



add :: Int -> Int -> Int
add x y = x + y



echoServer ::IO()
echoServer = do
       Right transport <- createTransport "localhost" "30179" defaultTCPParameters
       var <- newTMVarIO []
       transportServer transport $ server $ promoteTo $ runWeakBinary var
       return ()



main :: IO()
main = getArgs >>= main2

main2 [] = do 
   hSetBuffering stdout LineBuffering 
   hSetBuffering stderr LineBuffering 
   _ <- forkIO $ main2 ["server"]          
   threadDelay $ 1000 * 1000          
   main2 ["client"]                   


main2 ("client":_) = do
      Right transport <- createTransport "localhost" "30178" defaultTCPParameters
      Nat f <- transportClient transport $ encodeEndPointAddress "localhost" "30179" 0
      let s = monadClient f
      
      send s $ do
                 push 1
                 push 2
                 push 3
                 push 4
      r <- send s $ pop     
      print r
      return ()
      r1 <- send s $ do                                      
         return 3                                       
      print r1                                                
      send s $ do                                            
                push 8                                        
      r2 <- send s pop                                       
      print r2                                                
      r3 <- send s$ do                                       
                      push 9
                      rr1 <- add <$> pop <*> pop               
                      push 3
                      rr2 <- add <$> pure 1 <*> pop            
                      push 4
                      push 5
                      rr3 <- add <$> pure 5 <*> pure 5         
                      rr4 <- add <$> do 
                                       push 1 
                                       rr <- pop
                                       push 6
                                       push 7
                                       return rr
                                  <* do push 8
                                        push 8
                                 <*> do push 9
                                        push 10
                                        push 11
                                        pop
                      return (rr1,rr2,rr3,rr4)                       
      print r3                                                
 

main2 ("server":_) = do
      echoServer
      return()





