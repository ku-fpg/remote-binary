{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
 
import           Control.Natural
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import           Control.Remote.Monad.Binary
import           Data.Binary
import qualified Data.ByteString.Lazy as BS
import           Control.Remote.Monad.Transport
import           Network.Transport  hiding (send)
import           Network.Transport.TCP


data Command :: * where
   Push :: Int -> Command   -- PUSH

instance Binary Command where
   put (Push n) = put n
   get = do i <- get
            return $ Push i

data Procedure :: * -> * where
  Pop :: Procedure Int    -- POP

instance BinaryQ Procedure where
   getQ = do i <- get
             case i :: Word8 of
               0 -> return $ Fmap put Pop
   putQ (Pop)= put (0 :: Word8) 

   interpQ (Pop) = get





push :: Int -> RemoteMonad Command Procedure ()
push n = command $ Push n

pop :: RemoteMonad Command Procedure Int
pop = procedure $ Pop


dispatchWeakPacket :: WP.WeakPacket Command Procedure a -> IO a
dispatchWeakPacket (WP.Command (Push n)) = print $ "Push "++ (show n)
dispatchWeakPacket (WP.Procedure (Pop)) = do 
                                         print $ "Pop"
                                         return (5 ::Int)

dispatchStrongPacket :: SP.StrongPacket Command Procedure a -> IO a
dispatchStrongPacket (SP.Command (Push n) cmds) = do  
                                    print $ "Push " ++ (show n)
                                    dispatchStrongPacket cmds
dispatchStrongPacket (SP.Procedure Pop)  = do  
                                    print $ "Pop"
                                    return (6 :: Int)

dispatchStrongPacket (SP.Done) = return () 

dispatchApplicativePacket :: AP.ApplicativePacket Command Procedure a -> IO a
dispatchApplicativePacket (AP.Command (Push n))   =  print $ "Push " ++ (show n)
dispatchApplicativePacket (AP.Procedure Pop) = do 
                                             print $ "Pop"
                                             return (7 :: Int)

dispatchApplicativePacket (AP.Zip f a b)   = do 
                                                ra <- dispatchApplicativePacket a
                                                rb <- dispatchApplicativePacket b 
                                                return $ f ra rb
dispatchApplicativePacket (AP.Pure a)     = return a


runWeakBinary ::  WP.WeakPacket Command Procedure :~> IO
runWeakBinary =  nat dispatchWeakPacket

runStrongBinary :: SP.StrongPacket Command Procedure :~> IO
runStrongBinary = nat dispatchStrongPacket

runApplicativeBinary :: AP.ApplicativePacket Command Procedure :~> IO
runApplicativeBinary = nat dispatchApplicativePacket

add :: Int -> Int -> Int
add x y = x + y

echoServer ::IO()
echoServer = do
       Right transport <- createTransport "localhost" "30179" defaultTCPParameters
       transportServer transport $ server $ nat dispatchApplicativePacket
       return ()



main::IO()
main = do
        Right transport <- createTransport "localhost" "30178" defaultTCPParameters
        Nat f <- transportClient transport $ encodeEndPointAddress "localhost" "30179" 0
        let f1 = monadClient f
        r<- send f1 $ push 9 
        print r
        r<- send f1 $ pop 
{-

        putStrLn "Weak:"
        let f1 = weakSession runWeakBinary
        r <- send f1 $ do
                      push 9
                      pop
        print r
 
        putStrLn "Strong:"
        let f2 = strongSession runStrongBinary
        send f2 $ do
                    push 8
                    push 9
        r <- send f2 $ do 
                    push 5
                    push 6
                    pop
        print r

        putStrLn "Applicative:"
        let f3 = applicativeSession runApplicativeBinary

        r1 <- send f3 $ do
                 return 3
        print r1
        send f3 $ do
                  push 8
        r2 <- send f3 pop
        print r2
        r3 <- send f3$ do
                        r1 <- add <$> pop <*> pop
                        r2 <- add <$> pure 1 <*> pop
                        r3 <- add <$> pure 5 <*> pure 5
                        return (r1,r2,r3)
        print r3
-}
        return ()
