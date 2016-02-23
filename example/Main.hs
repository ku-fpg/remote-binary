{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
 
import           Control.Natural
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import           Control.Remote.Monad.Binary.Types
import           Control.Remote.Monad.Binary
import           Data.Binary


data Command :: * where
   Push :: Int -> Command   -- PUSH

instance Binary Command where
   put (Push n) = put n
   get = do i <- get
            return $ Push i

data Procedure :: * -> * where
  Pop :: Procedure Int    -- POP

instance Binary (T Procedure ) where
   put (T Pop) = put (0::Word8)
   get = do i <- get 
            case i :: Word8 of
               0 -> return $ T Pop

instance BinaryX Procedure



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

runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure) IO
runWeakBinary = BinaryNatTrans dispatchWeakPacket

runStrongBinary :: BinaryNatTrans (SP.StrongPacket Command Procedure) IO
runStrongBinary = BinaryNatTrans dispatchStrongPacket


main::IO()
main = do 
        putStrLn "Weak:"
        let f1 = receiveWeakSendAPI runWeakBinary
        sendWeakBinary f1 (WP.Command (Push 9):: WP.WeakPacket Command Procedure ())
        r <- sendWeakBinary f1 (WP.Procedure (Pop):: WP.WeakPacket Command Procedure Int)
        print r
 
        putStrLn "Strong:"
        let f2 = receiveStrongSendAPI runStrongBinary
        sendStrongBinary f2 (SP.Command (Push 8) (SP.Command (Push 7) (SP.Done)) :: SP.StrongPacket Command Procedure ())
        r <- sendStrongBinary f2 (SP.Command (Push 5) (SP.Command (Push 6) (SP.Procedure Pop)) :: SP.StrongPacket Command Procedure Int)
        print r


        return ()
