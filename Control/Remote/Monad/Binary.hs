{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

{-|
Module:      Control.Remote.Monad.Binary
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
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS


data Command :: * where
   Command :: Int -> Command   -- PUSH

instance Binary Command where
   put (Command n) = put n
   get = do i <- get
            return $ Command i

data Procedure :: * -> * where
  Procedure :: Procedure Int    -- POP

instance BinaryX Procedure


instance Binary (T Procedure ) where
   put (T Procedure) = put (0::Word8)
   get = do i <- get 
            case i :: Word8 of
               0 -> return $ T Procedure


sendWeakBinary :: (Binary a) => (SendAPI ~> IO) -> WP.WeakPacket Command Procedure a -> IO a
sendWeakBinary f pkt = do 
                        r <- f (Sync (runPut $ encodeWeakPacket  pkt))
                        return $ decodeWeakPacketResult pkt r 
                                             




runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO 
runWeakBinary = BinaryNatTrans  dispatchPacket

receiveSendAPI :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO -> (SendAPI ~> IO)
receiveSendAPI (BinaryNatTrans f) (Sync c) = do 
                     print c
                     case decode c of 
                       (T v{-@(WP.Command _c)-}) -> do  
                                     r <- f v
                                     return $ encodeWeakPacketResult v r
{-
receiveSendAPI (BinaryNatTrans f) (Sync c) = do 
                     print c
                     case decode c of
                       (T c') ->do  
                                   v <- f c'
                                   print v
                                   return $ encode v
-}
dispatchPacket :: WP.WeakPacket Command Procedure a -> IO a
dispatchPacket (WP.Command (Command n)) = print $ "Push "++ (show n)  
dispatchPacket (WP.Procedure (Procedure)) = do print $ "Pop"  
                                               return (5 ::Int)

main::IO()
main = do 
        let f1 = receiveSendAPI runWeakBinary
        sendWeakBinary f1 (WP.Command (Command 9))
        r <- sendWeakBinary f1 (WP.Procedure (Procedure))
        print r
        return ()
