{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ScopedTypeVariables #-} 


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
    , BinaryQ(..)
    , Fmap(..)
    , SendAPI(..)
    , weakSession
    , strongSession
    , applicativeSession
    , send
    , monadClient
    , server
    ) where
import           Control.Natural
import           Control.Remote.Monad
import           Control.Remote.Monad.Binary.Types
import  qualified Control.Remote.Monad.Packet.Weak as WP
import  qualified Control.Remote.Monad.Packet.Strong as SP
import  qualified Control.Remote.Monad.Packet.Applicative as AP
import           Data.Binary
import           Data.Binary.Put (runPut)
import           Data.Binary.Get (runGet)


sendBinaryQ :: (BinaryQ f) => (SendAPI ~> IO) -> f a -> IO a
sendBinaryQ f pkt = do 
                        r <- f (Sync (encodePacket  pkt))
                        return $ decodePacketResult pkt r 
                 


receiveSendAPI :: (BinaryQ f) => (f :~> IO) -> (SendAPI ~> IO)
receiveSendAPI (Nat f) (Sync c) = do 
                     case runGet getQ c of 
                       (Fmap f' v) -> do 
                                  r  <- f v 
                                  return $ runPut $ f' r

weakSession :: forall c p f . (f ~ WP.WeakPacket, BinaryQ (f c p), RunMonad f) => (WP.WeakPacket c p :~> IO ) -> (RemoteMonad c p  :~> IO)
weakSession  f =  runMonad g
           where
                g :: (WP.WeakPacket c p  :~> IO)
                g = nat $ sendBinaryQ  $ receiveSendAPI f

strongSession :: (BinaryQ p, Binary c) => (SP.StrongPacket c p :~>IO) -> (RemoteMonad c p  :~> IO)
strongSession  f =  runMonad f

applicativeSession :: (BinaryQ p, Binary c) => (AP.ApplicativePacket c p :~>IO) -> (RemoteMonad c p  :~> IO)
applicativeSession  f =  runMonad f


monadClient :: forall c p f . (f ~ AP.ApplicativePacket, BinaryQ (f c p))=> (SendAPI ~> IO) -> (RemoteMonad c p  :~> IO)
monadClient f = runMonad g
          where
               g :: (AP.ApplicativePacket c p :~> IO)
               g = nat $ sendBinaryQ f


server :: (Binary c, BinaryQ p)=> (AP.ApplicativePacket c p :~> IO ) -> (SendAPI :~> IO )
server f =  nat $ receiveSendAPI $ f



send :: (RemoteMonad c p :~> IO) -> (RemoteMonad c p a)-> IO a
send f m =  f # m 

