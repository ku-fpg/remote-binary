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

module Control.Remote.Monad.Binary
    ( sendBinaryQ
    , receiveSendAPI
    , sendStrongBinary
    , receiveStrongSendAPI
    , sendApplicativeBinary
    , receiveApplicativeSendAPI
    , SendAPI(..)
    ) where
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
sendBinaryQ :: (Binary a, BinaryQ f) => (SendAPI ~> IO) -> f a -> IO a
sendBinaryQ f pkt = do 
                        r <- f (Sync (encodePacket  pkt))
                        return $ decodePacketResult pkt r 
                                             


receiveSendAPI :: (BinaryQ f) => (f :~> IO) -> (SendAPI ~> IO)
receiveSendAPI (Nat f) (Sync c) = do 
                     case runGet getQ c of 
                       (RemotePacket v) -> do 
                                  r  <- f v 
                                  return $ encode r


                                  -- ##### Strong Packet ######

sendStrongBinary :: (Binary a, Binary c, BinaryQ p) => (SendAPI ~> IO) -> SP.StrongPacket c p a -> IO a
sendStrongBinary f pkt = do
                        r <- f (Sync (encodeStrongPacket pkt))
                        return $ decodeStrongPacketResult pkt r 

receiveStrongSendAPI :: (Binary c, BinaryQ p) => BinaryNatTrans (SP.StrongPacket c p)  IO -> (SendAPI ~> IO)
receiveStrongSendAPI (BinaryNatTrans f) (Sync c) = do
                                        case runGet getQ c of
                                          (RemotePacket v ) -> do
                                                    r <- f v 
                                                    return $ encode r

-- ##### Applicative Packet ######

sendApplicativeBinary :: (Binary a, Binary c, BinaryQ p) => (SendAPI ~> IO) -> AP.ApplicativePacket c p a -> IO a
sendApplicativeBinary f pkt = do
                            case pkt of
                              (AP.Pure a)    -> return a
                            {-  (AP.Zip f2 a b) -> do 
                                               rs <- f (Sync (encodeApplicativePacket pkt))
                                               return  $ (\(a, b) -> f2 a b) rs 
    -}
                              otherwise      -> do 
                                       r <- f (Sync (encodeApplicativePacket pkt))
                                       return $ decodeApplicativePacketResult pkt r

receiveApplicativeSendAPI :: (Binary c, BinaryQ p) => BinaryNatTrans (AP.ApplicativePacket c p)  IO -> (SendAPI ~> IO)
receiveApplicativeSendAPI (BinaryNatTrans f) (Sync c) = do
                                             case runGet getQ c of
                                                (RemotePacket v ) -> do
                                                       r <- f v
                                                       return $ encode r


