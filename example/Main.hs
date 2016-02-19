{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
 
import           Control.Natural
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
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



dispatchPacket :: WP.WeakPacket Command Procedure a -> IO a
dispatchPacket (WP.Command (Push n)) = print $ "Push "++ (show n)
dispatchPacket (WP.Procedure (Pop)) = do 
                                         print $ "Pop"
                                         return (5 ::Int)

runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure) IO
runWeakBinary = BinaryNatTrans dispatchPacket



main::IO()
main = do 
        let f1 = receiveSendAPI runWeakBinary
        sendWeakBinary f1 (WP.Command (Push 9):: WP.WeakPacket Command Procedure ())
        r <- sendWeakBinary f1 (WP.Procedure (Pop):: WP.WeakPacket Command Procedure Int)
        print r
        return ()
