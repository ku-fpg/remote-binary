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
    ( BinaryQ(..)
    , Fmap(..)
    , SendAPI(..)
    , send
    , monadClient
    , server
    , RemoteBinaryException(..)
    ) where
import           Control.Natural
import           Control.Remote.Monad
import           Control.Remote.Monad.Binary.Types
import  qualified Control.Remote.Monad.Packet.Applicative as AP
import           Data.Binary
import           Data.Binary.Put (runPut)
import           Data.Binary.Get (runGet)
import           Control.Exception

-- Internal sending of BinaryQ object
sendBinaryQ :: (BinaryQ f) => (SendAPI ~> IO) -> f a -> IO a
sendBinaryQ f pkt = do
                        r <- f (Sync (encodePacket  pkt))
                        case decodePacketResult pkt r of
                           Left e -> throw e
                           Right a -> return a

-- | This function is used to convert a function that can transport ByteStrings in a SendAPI wrapper to a function
--   that can use the remote-monad bundling strategies and then send them via the input function.
monadClient :: forall c p f . (f ~ AP.ApplicativePacket, BinaryQ (f c p))=> (SendAPI ~> IO) -> (RemoteMonad c p  :~> IO)
monadClient f = runMonad g
          where
               g :: (AP.ApplicativePacket c p :~> IO)
               g = wrapNT $ sendBinaryQ f


-- internal receiving of sync object
receiveSendAPI :: (BinaryQ f) => (f :~> IO) -> (SendAPI ~> IO)
receiveSendAPI (NT f) (Sync c) = do
                     case runGet getQ c of
                       (Fmap f' v) -> do
                                  r::Either SomeException a  <- try $ f v

                                  case r of
                                    Right res -> return $runPut $ wrapSuccess (f' res)
                                    Left  e -> return $ runPut $ wrapError (put $ RemoteBinaryException (displayException e))


wrapError :: Put -> Put
wrapError m = put (1::Word8) >> m
wrapSuccess :: Put -> Put
wrapSuccess m = put (0::Word8) >> m

-- | This function takes a function that can execute a remote-monad packet containing the User's GADT
--   and elevates it to handle the encoding of the response
server :: (Binary c, BinaryQ p)=> (AP.ApplicativePacket c p :~> IO ) -> (SendAPI :~> IO )
server f =  wrapNT $ receiveSendAPI $ f

-- | send remote monad , equivalent to executing the natural transformation on the RemoteMonad
send :: (RemoteMonad c p :~> IO) -> (RemoteMonad c p a)-> IO a
send f m =  f # m
