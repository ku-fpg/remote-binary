{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverloadedStrings, ExistentialQuantification #-}




{-|                                             
 - Module:      Control.Remote.Monad.JSON where
 - Copyright:   (C) 2015, The University of Kansas
 - License:     BSD-style (see the file LICENSE)
 - Maintainer:  Justin Dawson                      
 - Stability:   Alpha                              
 - Portability: GHC                                
 - -}


 module Control.Remote.Monad.Binary.Types (
   BinaryNatTrans(..)
 , Command(..)
 , Procedure(..)
 , RemoteBinary(..)
 , SendAPI(..)
 , Session(..)
 , T(..)
 ) where

import Control.Natural
import Control.Monad (void)
import Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import Data.ByteString.Lazy
import Data.Binary

data Command :: * where
   Command :: Int -> Command

instance Binary Command where
   put (Command n) = put n
   get = do i <- get 
            return $ Command i

data Procedure :: * -> * where
  Procedure :: Procedure Int



data SendAPI :: * -> * where
    Sync  :: ByteString -> SendAPI (Maybe ByteString)
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
                  
{-
transportSendAPI f (Async c) = do 
                               let send_data = encode $ T (Async c) 
                               case  decode send_data of
                                  (T (Sync x))  ->  error "SendAPI error"
                                  (T (Async x)) -> do
                                                    () <- f (Async x)
                                                    return () 
  -}                             
{- 
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
-}

newtype RemoteBinary a = RemoteBinary (RemoteMonad Command Procedure a)

newtype Session = Session (RemoteMonad Command Procedure :~> IO)

data T f where
  T:: (Binary a) => f a -> T f

data BinaryNatTrans f g = BinaryNatTrans (forall a. (Binary a) => f a -> g a)

instance Binary (T (WP.WeakPacket Command Procedure)) where
      put (T (WP.Command c)) = do put (0 :: Word8)
                                  put c

      get = do i <- get
               case i :: Word8 of
                 0 -> ( T . WP.Command )<$> get



