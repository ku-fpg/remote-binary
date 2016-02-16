{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

{-|
Module:      Control.Remote.Monad.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Binary (




) where
import           Control.Monad (void)
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary.Types
import           Data.Binary

instance Binary (T (WP.WeakPacket Command Procedure)) where
      put (T (WP.Command c)) = do put (0 :: Word8)
                                  put c

      get = do i <- get
               case i :: Word8 of
                 0 -> ( T . WP.Command )<$> get


sendWeakBinary :: (SendAPI ~> IO) -> WP.WeakPacket Command Procedure a -> IO a
sendWeakBinary f pkt@(WP.Command c)   = f (Async (encode (T pkt)))


data BinaryNatTrans f g = BinaryNatTrans (forall a. (Binary a) => f a -> g a)


runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO 
runWeakBinary = BinaryNatTrans $ \ (WP.Command c) -> runCommand c

receiveSendAPI :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO -> (SendAPI ~> IO)
receiveSendAPI (BinaryNatTrans f) (Async c) = do 
                     print c
                     case decode c of 
                       (T c') -> void $f c'

runCommand :: Command -> IO ()
runCommand (Command n) = print $ "Push "++ (show n)  


main::IO()
main = do 
        let f1 = receiveSendAPI runWeakBinary
        sendWeakBinary f1 (WP.Command (Command 9))
