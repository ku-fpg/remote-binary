{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverloadedStrings, ExistentialQuantification, InstanceSigs #-}




{-|                                             
 - Module:      Control.Remote.Monad.Binary.Types 
 - Copyright:   (C) 2015, The University of Kansas
 - License:     BSD-style (see the file LICENSE)
 - Maintainer:  Justin Dawson                      
 - Stability:   Alpha                              
 - Portability: GHC                                
 - -}


 module Control.Remote.Monad.Binary.Types 
-- (
--   BinaryNatTrans(..)
-- , BinaryX
-- , encodeWeakPacket
-- , decodeWeakPacketResult
-- , encodeWeakPacketResult
-- , encodeStrongPacket
-- , decodeStrongPacketResult
-- , encodeStrongPacketResult
-- , encodeApplicativePacket
-- , decodeApplicativePacketResult
-- , encodeApplicativePacketResult
-- , SendAPI(..)
-- , T(..)
-- )
 where

import Control.Natural
import Control.Monad (void)
import Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)


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


data T' f where
  T' :: f a -> T' f

-- ############## Weak Packet #######################

instance (Binary c, BinaryQ p) => BinaryQ (WP.WeakPacket c p) where
  putQ = putWeakPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do 
                c <- get
                return $ Query put (WP.Command c)
          1 -> do 
                (Query f p) <- getQ
                return $ Query f (WP.Procedure p)

  interpQ (WP.Command c)   = return ()
  interpQ (WP.Procedure p) = interpQ p


putWeakPacket :: (Binary c, BinaryQ p) => WP.WeakPacket c p a -> Put
putWeakPacket (WP.Command c) = do
   put (0 :: Word8)
   put c
putWeakPacket (WP.Procedure p)= do
   put (1 :: Word8)
   putQ p



encodeWeakPacket :: (Binary c, BinaryQ p)=> WP.WeakPacket c p a -> ByteString 
encodeWeakPacket pkt = runPut (putQ pkt) 

decodeWeakPacketResult :: (BinaryQ p, Binary c, Binary a) => WP.WeakPacket c p a -> ByteString -> a
decodeWeakPacketResult pkt = runGet (interpQ pkt)

encodeWeakPacketResult :: (BinaryQ p, Binary a) => WP.WeakPacket c p a ->  a -> ByteString
encodeWeakPacketResult (WP.Procedure p) = undefined

---- ############ Strong Packet ##########################

instance (Binary c, BinaryQ p) => BinaryQ (SP.StrongPacket c p) where
  putQ = putStrongPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do 
                c <- get
                (Query f cmds) <- getQ
                return $ Query f (SP.Command c cmds)
          1 -> do 
                (Query f p) <- getQ
                return $ Query f (SP.Procedure p)
          2 -> do
                return $ Query put (SP.Done)

  interpQ (SP.Command c cmds)   = interpQ cmds
  interpQ (SP.Procedure p) = interpQ p


putStrongPacket :: (Binary c, BinaryQ p) => SP.StrongPacket c p a -> Put
putStrongPacket (SP.Command c cmds) = do
   put (0 :: Word8)
   put c
   putStrongPacket cmds
putStrongPacket (SP.Procedure p)= do
   put (1 :: Word8)
   putQ p
putStrongPacket (SP.Done) = put (2 :: Word8)

encodeStrongPacket :: (Binary c, BinaryQ p)=> SP.StrongPacket c p a -> ByteString 
encodeStrongPacket pkt = runPut (putQ pkt) 

decodeStrongPacketResult :: (BinaryQ p, Binary c, Binary a) => SP.StrongPacket c p a -> ByteString -> a
decodeStrongPacketResult pkt = runGet (interpQ pkt)

encodeStrongPacketResult :: (BinaryQ p, Binary a) => SP.StrongPacket c p a ->  a -> ByteString
encodeStrongPacketResult (SP.Procedure p) = undefined

--instance (Binary c, BinaryX p) => Binary (T (SP.StrongPacket c p)) where
--      put (T (SP.Command c cmds)) = do   
--                          put ( 0 :: Word8)
--                          put c
--                          put (T cmds)
--
--      put (T (SP.Procedure proc)) = do 
--                          put ( 1 :: Word8)
--                          encodeProcedure proc
--
--      put (T (SP.Done)) = put ( 2 :: Word8)
--      
--      get = do 
--              i <- get
--              case i :: Word8 of
--                0 -> do a <- get
--                        (T b) <- get
--                        return $ T ( SP.Command a b )
--                1 -> (\(T p ) -> T $ SP.Procedure $ p) <$> get
--                2 -> return $ T SP.Done
--
--
--encodeStrongPacket :: (Binary c, Binary a, BinaryX p) => SP.StrongPacket c p a -> ByteString 
--encodeStrongPacket pkt =encode (T pkt) 
--
--encodeStrongPacketResult :: (BinaryX p, Binary a) => SP.StrongPacket c p a ->  a -> ByteString
--encodeStrongPacketResult (SP.Procedure p)= encodeProcedureResult p
--encodeStrongPacketResult (SP.Command c cmds)= encodeStrongPacketResult cmds
--
--
--decodeStrongPacketResult :: (BinaryX p, Binary a) => SP.StrongPacket c p a -> ByteString -> a
--decodeStrongPacketResult (SP.Procedure p)   = decodeProcedureResult p 
--decodeStrongPacketResult (SP.Command c cmds)= decodeStrongPacketResult cmds 
--decodeStrongPacketResult (SP.Done)          = const ()
--
--
---- ############ Applicative Packet #####################
--instance (Binary c, BinaryX p) => Binary (T (AP.ApplicativePacket c p)) where
--     put ( T (AP.Command c )) = do
--                           put (0 :: Word8)
--                           put c
--
--     put ( T (AP.Procedure proc )) = do
--                           put (1 :: Word8)
--                           encodeProcedure proc
--     --TODO Encode Zip (Type Errors currently)              
--     put ( T (AP.Zip f a b)) = do
--                           put (2 :: Word8)
----                           put (T' a)
----                           put (T' b)
--  
--     put ( T (AP.Pure a )) = undefined
--    
--     get = do
--         i <- get
--         case i :: Word8 of
--            0 -> (T . AP.Command) <$> get
--            1 -> (\(T p ) -> T $ AP.Procedure $ p) <$> get
--            2 -> do (T a) <- get
--                    (T b) <- get
--                    --TODO This function is to allow a return of a list of bytestrings from server
--                    return $ T (AP.Zip (\ x y -> [encode x, encode y] ) a b)
--            3 -> undefined 
--{-
--instance (Binary c, BinaryX p) => Binary (T' (AP.ApplicativePacket c p)) where
--     put (T' (AP.Command c )) = do
--                           put (0 :: Word8)
--                           put c
--
--     put (T' p@(AP.Procedure proc)) = put (T p)
--     
--     --TODO Encode Zip (Type Errors currently)              
--     put (T' (AP.Zip f a b)) = do
--                           put (2 :: Word8)
--                           put (T' a)
--                           put (T' b)
--  
--     put (T' (AP.Pure a )) = undefined
--    
--     get = undefined
---}
{- 

instance (Binary c, BinaryQ p) => BinaryQ (AP.ApplicativePacket c p) where
  putQ = putApplicativePacket 

putApplicativePacket :: (Binary c, BinaryQ p) => AP.ApplicativePacket c p a -> Put
putApplicativePacket (AP.Command c) = do
    put (0 :: Word8)
    put c
putApplicativePacket (AP.Procedure p) = do
    put (1 :: Word8)
    putQ p
putApplicativePacket (AP.Zip  _ a b) = do
    put (2 :: Word8)
    putApplicativePacket a
    putApplicativePacket b
putApplicativePacket (AP.Pure _) = do
    put (3 :: Word8)


instance (Binary c, BinaryGetX p) => BinaryGetX (AP.ApplicativePacket c p) where
    getQ = do
         i <- get
         case i :: Word8 of
            0 -> do
                 c <- get
                 return $ GetX put (AP.Command c)
-}
{-
            1 -> (\(T p ) -> T $ AP.Procedure $ p) <$> get
            2 -> do (T a) <- get
                    (T b) <- get
                    --TODO This function is to allow a return of a list of bytestrings from server
                    return $ T (AP.Zip (\ x y -> [encode x, encode y] ) a b)
            3 -> undefined 
-}
{-
instance (BinaryGetReplyX p) => BinaryGetReplyX (AP.ApplicativePacket c p) where
  interpQ (AP.Command c)   = return ()
  interpQ (AP.Procedure p) = interpQ p
  interpQ (AP.Zip f x y)   = f <$> interpQ x <*> interpQ y
  interpQ (AP.Pure a)      = return a
-}
--          
--encodeApplicativePacket :: (Binary c, Binary a, BinaryX p) => AP.ApplicativePacket c p a -> ByteString 
--encodeApplicativePacket pkt = encode (T pkt)
--
--encodeApplicativePacketResult :: (BinaryX p, Binary a) => AP.ApplicativePacket c p a ->  a -> ByteString
--encodeApplicativePacketResult (AP.Procedure p) = encodeProcedureResult p
--
--decodeApplicativePacketResult :: (BinaryX p, Binary a) => AP.ApplicativePacket c p a -> ByteString -> a
--decodeApplicativePacketResult (AP.Procedure p)= decodeProcedureResult p
--decodeApplicativePacketResult (AP.Zip f a b)=  --TODO somehow convert ByteString -> to input parameters for f 

data Query (p :: * -> *) where
   Query :: (Binary a) => (a -> Put) -> p a -> Query p

class BinaryQ p  where
  putQ    :: p a -> Put     -- encode a query/question
  getQ    :: Get (Query p)  -- decode to the query, which contains a way of encoding the answer
  interpQ :: p a -> Get a   -- interprete the answer, in the context of the original query (type).

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
