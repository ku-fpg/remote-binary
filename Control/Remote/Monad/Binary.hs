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
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import           Control.Remote.Monad.Binary.Types
import           Data.Binary
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS


-- ##### Weak Packet  ######
sendWeakBinary :: (Binary a, Binary c,BinaryGetReplyX p, BinaryPutX p) => (SendAPI ~> IO) -> WP.WeakPacket c p a -> IO a
sendWeakBinary f pkt = do 
                        r <- f (Sync (encodeWeakPacket  pkt))
                        return $ decodeWeakPacketResult pkt r 
                                             


receiveWeakSendAPI :: (Binary c, BinaryGetX p, BinaryGetReplyX p) => BinaryNatTrans (WP.WeakPacket c p)  IO -> (SendAPI ~> IO)
receiveWeakSendAPI (BinaryNatTrans f) (Sync c) = do 
                     case runGet getX c of 
                       (GetX f' v) -> do  
                                  r  <- f v 
                                  return $ runPut (f' r) 

-- ##### Strong Packet ######
--
--sendStrongBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> SP.StrongPacket c p a -> IO a
--sendStrongBinary f pkt = do
--                        r <- f (Sync (encodeStrongPacket pkt))
--                        return $ decodeStrongPacketResult pkt r 
--
--receiveStrongSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (SP.StrongPacket c p)  IO -> (SendAPI ~> IO)
--receiveStrongSendAPI (BinaryNatTrans f) (Sync c) = do
--                                        case decode c of
--                                          (T v ) -> do
--                                                    r <- f v
--                                                    return $ encodeStrongPacketResult v r
--
---- ##### Applicative Packet ######
--
--sendApplicativeBinary :: (Binary a, Binary c, BinaryX p) => (SendAPI ~> IO) -> AP.ApplicativePacket c p a -> IO a
--sendApplicativeBinary f pkt = do
--                            case pkt of
--                              (AP.Pure a)    -> return a
--   {-TODO                           (AP.Zip f2 a b) -> do 
--                                                rs <- f (Sync (encodeApplicativePacket pkt))
--                                                return $ foldl1 f2 rs 
--   -} 
--                              otherwise      -> do 
--                                       r <- f (Sync (encodeApplicativePacket pkt))
--                                       return $ decodeApplicativePacketResult pkt r
--
--receiveApplicativeSendAPI :: (Binary c, BinaryX p) => BinaryNatTrans (AP.ApplicativePacket c p)  IO -> (SendAPI ~> IO)
--receiveApplicativeSendAPI (BinaryNatTrans f) (Sync c) = do
--                                             case decode c of
--                                                (T v ) -> do
--                                                       r <- f v
--                                                       return $ encodeApplicativePacketResult v r

