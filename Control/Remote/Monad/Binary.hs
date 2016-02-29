{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE TypeOperators #-} 

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
    , SendAPI(..)
    ) where
import           Control.Natural
import           Control.Remote.Monad.Binary.Types
import           Data.Binary
import           Data.Binary.Get (runGet)


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

