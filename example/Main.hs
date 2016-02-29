{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
 
import           Control.Natural
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import           Control.Remote.Monad.Binary.Types
import           Control.Remote.Monad.Binary
import           Data.Binary
import qualified Data.ByteString.Lazy as BS


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
               0 -> return $ RemotePacket Pop
   putQ (Pop)= put (0 :: Word8) 

   interpQ (Pop) = get



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


runWeakBinary :: WP.WeakPacket Command Procedure :~> IO
runWeakBinary =  nat dispatchWeakPacket

runStrongBinary :: SP.StrongPacket Command Procedure :~> IO
runStrongBinary = nat dispatchStrongPacket

runApplicativeBinary :: AP.ApplicativePacket Command Procedure :~> IO
runApplicativeBinary = nat dispatchApplicativePacket

add :: Int -> Int -> Int
add x y = x + y

main::IO()
main = do
        putStrLn "Weak:"
        let f1 = receiveSendAPI runWeakBinary
        sendBinaryQ f1 (WP.Command (Push 9):: WP.WeakPacket Command Procedure ())
        r <- sendBinaryQ f1 (WP.Procedure (Pop):: WP.WeakPacket Command Procedure Int)
        print r
 
        putStrLn "Strong:"
        let f2 = receiveSendAPI runStrongBinary
        sendBinaryQ f2 (SP.Command (Push 8) (SP.Command (Push 7) (SP.Done)) :: SP.StrongPacket Command Procedure ())
        r <- sendBinaryQ f2 (SP.Command (Push 5) (SP.Command (Push 6) (SP.Procedure Pop)) :: SP.StrongPacket Command Procedure Int)
        print r

        putStrLn "Applicative:"
        let f3 = receiveSendAPI runApplicativeBinary

        r1 <- sendBinaryQ f3 (AP.Pure (3) :: AP.ApplicativePacket Command Procedure Int)
        print r1
        sendBinaryQ f3 (AP.Command (Push 8) :: AP.ApplicativePacket Command Procedure ())
        r2 <- sendBinaryQ f3 (AP.Procedure (Pop) :: AP.ApplicativePacket Command Procedure Int)
        print r2
        r3 <- sendBinaryQ f3 (AP.Zip (add) (AP.Procedure (Pop)) (AP.Procedure (Pop)) :: AP.ApplicativePacket Command Procedure Int)
        print r3

        return ()
