{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
 
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary.Types
import           Control.Remote.Monad.Binary
import           Data.Binary


data Command :: * where
   Command :: Int -> Command   -- PUSH

instance Binary Command where
   put (Command n) = put n
   get = do i <- get
            return $ Command i

data Procedure :: * -> * where
  Procedure :: Procedure Int    -- POP

instance Binary (T Procedure ) where
   put (T Procedure) = put (0::Word8)
   get = do i <- get 
            case i :: Word8 of
               0 -> return $ T Procedure

instance BinaryX Procedure



dispatchPacket :: WP.WeakPacket Command Procedure a -> IO a
dispatchPacket (WP.Command (Command n)) = print $ "Push "++ (show n)
dispatchPacket (WP.Procedure (Procedure)) = do print $ "Pop"
                                               return (5 ::Int)

runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure) IO
runWeakBinary = BinaryNatTrans dispatchPacket


main::IO()
main = do 
        let f1 = receiveSendAPI runWeakBinary
        sendWeakBinary f1 (WP.Command (Command 9):: WP.WeakPacket Command Procedure ())
        r <- sendWeakBinary f1 (WP.Procedure (Procedure):: WP.WeakPacket Command Procedure Int)
        print r
        return ()
