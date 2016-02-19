{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverloadedStrings, ExistentialQuantification #-}




{-|                                             
 - Module:      Control.Remote.Monad.Binary.Types 
 - Copyright:   (C) 2015, The University of Kansas
 - License:     BSD-style (see the file LICENSE)
 - Maintainer:  Justin Dawson                      
 - Stability:   Alpha                              
 - Portability: GHC                                
 - -}


 module Control.Remote.Monad.Binary.Types (
   BinaryNatTrans(..)
 , BinaryX
 , encodeWeakPacket
 , decodeWeakPacketResult
 , encodeWeakPacketResult
 , SendAPI(..)
 , T(..)
 ) where

import Control.Natural
import Control.Monad (void)
import Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import Data.ByteString.Lazy
import Data.Binary


data SendAPI :: * -> * where
    Sync  :: ByteString -> SendAPI ByteString
--    Async :: ByteString -> SendAPI ()

instance Binary (T SendAPI) where
    put (T (Sync p) ) = do put (0 :: Word8)
                           put p
{-    put (T (Async c) ) = do put (1 :: Word8)
                            put c
 -}
    get = do i <- get 
             case i ::Word8 of
               0 ->  (T . Sync)  <$> get 
--               1 ->  (T . Async) <$> get

transportSendAPI :: (SendAPI ~> IO) -> SendAPI ~> IO
transportSendAPI f (Sync c) =  do 
                               let send_data = encode $ T (Sync c) 
                               case  decode send_data of
--                                  (T (Async x))-> error "SendAPI error"
                                  (T (Sync x)) -> do
                                                  a <- f (Sync x)
                                                  let reply_data = encode a
                                                  let reply_value = decode reply_data
                                                  return reply_value
data T f where
  T:: (Binary a) => f a -> T f

data BinaryNatTrans f g = BinaryNatTrans (forall a. (Binary a) => f a -> g a)

instance (BinaryX p, Binary c)=>Binary (T (WP.WeakPacket c p)) where
      put (T (WP.Command com)) = do 
                                  put (0 :: Word8)
                                  put com
          
      put (T (WP.Procedure proc)) = do 
                                  put (1 :: Word8)
                                  put (T proc)

      get = do i <- get
               case i :: Word8 of
                 0 -> ( T . WP.Command )  <$> get
                 1 -> (\(T p) -> T $ WP.Procedure  $ p)<$> get


encodeWeakPacket :: (Binary c, Binary a, BinaryX p) => WP.WeakPacket c p a -> Put 
encodeWeakPacket (WP.Procedure p) = do
                                        put (1 :: Word8)
                                        encodeProcedure p

encodeWeakPacket c@(WP.Command {}) = put (T c)


decodeWeakPacketResult :: (BinaryX p, Binary a) => WP.WeakPacket c p a -> ByteString -> a
decodeWeakPacketResult (WP.Procedure p) = decodeProcedureResult p

encodeWeakPacketResult :: (BinaryX p, Binary a) => WP.WeakPacket c p a ->  a -> ByteString
encodeWeakPacketResult (WP.Procedure p) = encodeProcedureResult p


class (Binary (T p)) => BinaryX p  where
  decodeProcedureResult :: (Binary a) =>  p a -> ByteString -> a
  decodeProcedureResult p = decode

  encodeProcedureResult :: (Binary a) => p a -> a -> ByteString
  encodeProcedureResult p = encode 

  encodeProcedure:: (BinaryX p, Binary a) => p a -> Put
  encodeProcedure p = put (T p)




-- Artifacts

{-
newtype RemoteBinary a = RemoteBinary (RemoteMonad Command Procedure a)

newtype Session = Session (RemoteMonad Command Procedure :~> IO)


 
instance Binary (T ReceiveAPI) where
    put (T (Receive p) ) = do put (0 :: Word8)
                              put p
    get = do i <- get
             case i :: Word8 of
               0 -> (T . Receive) <$> get

deriving instance Show (SendAPI a)

data ReceiveAPI :: * -> * where
    Receive :: ByteString -> ReceiveAPI (Maybe ByteString)

deriving instance Show (ReceiveAPI a)

                  

transportSendAPI f (Async c) = do 
                               let send_data = encode $ T (Async c) 
                               case  decode send_data of
                                  (T (Sync x))  ->  error "SendAPI error"
                                  (T (Async x)) -> do
                                                    () <- f (Async x)
                                                    return () 
-}                             
