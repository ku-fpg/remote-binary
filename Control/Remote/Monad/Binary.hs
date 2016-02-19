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

module Control.Remote.Monad.Binary  where
import           Control.Monad (void)
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary.Types
import           Data.Binary
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS



sendWeakBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> WP.WeakPacket c p a -> IO a
sendWeakBinary f pkt = do 
                        r <- f (Sync (runPut $ encodeWeakPacket  pkt))
                        return $ decodeWeakPacketResult pkt r 
                                             



{-
runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO 
runWeakBinary = BinaryNatTrans  dispatchPacket
-}

receiveSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (WP.WeakPacket c p)  IO -> (SendAPI ~> IO)
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
