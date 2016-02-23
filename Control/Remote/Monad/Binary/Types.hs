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
 , encodeStrongPacket
 , decodeStrongPacketResult
 , encodeStrongPacketResult
 , SendAPI(..)
 , T(..)
 ) where

import Control.Natural
import Control.Monad (void)
import Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
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

-- ############## Weak Packet #######################

instance (BinaryX p, Binary c)=>Binary (T (WP.WeakPacket c p)) where
      put (T (WP.Command com)) = do 
                                  put (0 :: Word8)
                                  put com
          
      put (T (WP.Procedure proc)) = do 
                                  put (1 :: Word8)
                                  encodeProcedure proc

      get = do i <- get
               case i :: Word8 of
                 0 -> ( T . WP.Command )  <$> get
                 1 -> (\(T p) -> T $ WP.Procedure  $ p)<$> get


encodeWeakPacket :: (Binary c, Binary a, BinaryX p) => WP.WeakPacket c p a -> ByteString 
encodeWeakPacket pkt = encode  (T pkt)

encodeWeakPacketResult :: (BinaryX p, Binary a) => WP.WeakPacket c p a ->  a -> ByteString
encodeWeakPacketResult (WP.Procedure p) = encodeProcedureResult p

decodeWeakPacketResult :: (BinaryX p, Binary a) => WP.WeakPacket c p a -> ByteString -> a
decodeWeakPacketResult (WP.Procedure p) = decodeProcedureResult p


-- ############ Strong Packet ##########################
instance (Binary c, BinaryX p) => Binary (T (SP.StrongPacket c p)) where
      put (T (SP.Command c cmds)) = do   
                          put ( 0 :: Word8)
                          put c
                          put (T cmds)

      put (T (SP.Procedure proc)) = do 
                          put ( 1 :: Word8)
                          encodeProcedure proc

      put (T (SP.Done)) = put ( 2 :: Word8)
      
      get = do 
              i <- get
              case i :: Word8 of
                0 -> do a <- get
                        (T b) <- get
                        return $ T ( SP.Command a b )
                1 -> (\(T p ) -> T $ SP.Procedure $ p) <$> get
                2 -> return $ T SP.Done


encodeStrongPacket :: (Binary c, Binary a, BinaryX p) => SP.StrongPacket c p a -> ByteString 
encodeStrongPacket pkt =encode (T pkt) 

encodeStrongPacketResult :: (BinaryX p, Binary a) => SP.StrongPacket c p a ->  a -> ByteString
encodeStrongPacketResult (SP.Procedure p)= encodeProcedureResult p
encodeStrongPacketResult (SP.Command c cmds)= encodeStrongPacketResult cmds


decodeStrongPacketResult :: (BinaryX p, Binary a) => SP.StrongPacket c p a -> ByteString -> a
decodeStrongPacketResult (SP.Procedure p)   = decodeProcedureResult p 
decodeStrongPacketResult (SP.Command c cmds)= decodeStrongPacketResult cmds 
decodeStrongPacketResult (SP.Done)          = const ()


-- ############ Applicative Packet #####################
instance (Binary c, BinaryX p) => Binary (T (AP.ApplicativePacket c p)) where
     put ( T (AP.Command c )) = do
                           put (0 :: Word8)
                           put c

     put ( T (AP.Procedure proc )) = do
                           put (1 :: Word8)
                           encodeProcedure proc
                           
     put ( T (AP.Zip f a b)) = undefined

     put ( T (AP.Pure a )) = do
                           put (3 :: Word8)
    
     get = do
         i <- get
         case i :: Word8 of
            0 -> (T . AP.Command) <$> get
            1 -> (\(T p ) -> T $ AP.Procedure $ p) <$> get
            2 -> undefined
            3 -> undefined 

encodeApplicativePacket :: (Binary c, Binary a, BinaryX p) => AP.ApplicativePacket c p a -> ByteString 
encodeApplicativePacket pkt = encode (T pkt)

encodeApplicativePacketResult :: (BinaryX p, Binary a) => AP.ApplicativePacket c p a ->  a -> ByteString
encodeApplicativePacketResult (AP.Procedure p) = encodeProcedureResult p

decodeApplicativePacketResult :: (BinaryX p, Binary a) => AP.ApplicativePacket c p a -> ByteString -> a
decodeApplicativePacketResult (AP.Procedure p)= decodeProcedureResult p



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
