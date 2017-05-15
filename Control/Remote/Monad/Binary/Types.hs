{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}


{-|
 - Module:      Control.Remote.Monad.Binary.Types
 - Copyright:   (C) 2015, The University of Kansas
 - License:     BSD-style (see the file LICENSE)
 - Maintainer:  Justin Dawson
 - Stability:   Alpha
 - Portability: GHC
 - -}


 module Control.Remote.Monad.Binary.Types
 (
   encodePacket
 , decodePacketResult
 , BinaryQ(..)
 , Fmap(..)
 , SendAPI(..)
 , RemoteBinaryException(..)
 )
 where

import           Control.Exception
import qualified Control.Remote.Packet.Applicative as AP
--import qualified Control.Remote.Packet.Strong      as SP
import qualified Control.Remote.Packet.Weak        as WP
import           Data.Binary
import           Data.Binary.Get                   (runGet)
import           Data.Binary.Put                   (runPut)
import           Data.ByteString.Lazy
import           Data.Typeable

-------------

data SendAPI :: * -> * where
    Sync  :: ByteString -> SendAPI ByteString

data Fmap f a where
   Fmap :: (a -> b) -> f a -> Fmap f b

instance Functor (Fmap f) where
  fmap f (Fmap g h) = Fmap (f . g) h


class BinaryQ p  where
  putQ    :: p a -> Put            -- ^ encode a query/question as a packet
  getQ    :: Get (Fmap p Put)  -- ^ decode to the packet, which contains a way of encoding the answer
  interpQ :: p a -> Get a          -- ^ interprete the answer, in the context of the original query (type).

interpQ' :: (BinaryQ p, Exception e, Binary e) => p a -> Get (Either e a)
interpQ' pkt = do i <- get
                  case i :: Word8 of
                      0 -> do res <- interpQ pkt
                              return $ Right res
                      1 -> do e <- get
                              return $ Left e
                      _ -> error "Expected Error bit, but received some other value"

-------------
-- ############## Weak Packet BinaryQ Instance #######################

instance (BinaryQ p) => BinaryQ (WP.WeakPacket p) where
  putQ = putWeakPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do
                (Fmap f p) <- getQ
                return $ Fmap f (WP.Primitive  p)
          _ -> error "ERROR: function getQ unable to parse WeakPacket"

  interpQ (WP.Primitive p)  =  interpQ p

putWeakPacket :: (BinaryQ p) => WP.WeakPacket p a -> Put
putWeakPacket (WP.Primitive p)= do
   put (0 :: Word8)
   putQ p


---- ############ Strong Packet BinaryQ Instance ##########################
{-
instance (Binary c, BinaryQ p) => BinaryQ (SP.StrongPacket c p) where
  putQ = putStrongPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do
                c <- get
                Fmap f cmds <- getQ
                return $ Fmap f $ SP.Command c cmds
          1 -> do
                Fmap f p <- getQ
                return $ Fmap  f $ SP.Procedure p
          2 -> return $ Fmap (\() -> return ()) $ SP.Done
          _ -> error "ERROR: getQ unable to parse StrongPacket"

  interpQ (SP.Command _c cmds)   = interpQ cmds
  interpQ (SP.Procedure p) = interpQ p
  interpQ (SP.Done) =  return ()

putStrongPacket :: (Binary c, BinaryQ p) => SP.StrongPacket c p a -> Put
putStrongPacket (SP.Command c cmds) = do
   put (0 :: Word8)
   put c
   putStrongPacket cmds
putStrongPacket (SP.Procedure p)= do
   put (1 :: Word8)
   putQ p
putStrongPacket (SP.Done) = put (2 :: Word8)
-}
---- ############ Applicative Packet BinaryQ Instance #####################
instance (BinaryQ p) => BinaryQ (AP.ApplicativePacket p) where
  putQ = putApplicativePacket

  getQ = do
      i <- get
      case i :: Word8 of
         0 -> do
                Fmap f p <- getQ
                return $Fmap f $ AP.Primitive $ p

         1 -> do Fmap f1 q1 <- getQ
                 Fmap f2 q2 <- getQ
                 return $ Fmap (\(a,b)-> f1 a >> f2 b )$ AP.Zip (\ a b -> (a,b)) q1 q2
         2 -> return $ Fmap (\()-> return ()) $ AP.Pure ()

         _ -> error "ERROR: getQ unable to parse ApplicativePacket"

  interpQ (AP.Primitive p) = interpQ p
  interpQ (AP.Zip f x y)   = f <$> interpQ x <*> interpQ y
  interpQ (AP.Pure a)      = return a


putApplicativePacket :: (BinaryQ p) => AP.ApplicativePacket p a -> Put
putApplicativePacket (AP.Primitive p) = do
    put (0 :: Word8)
    putQ p
putApplicativePacket (AP.Zip  _ a b) = do
    put (1 :: Word8)
    putApplicativePacket a
    putApplicativePacket b
putApplicativePacket (AP.Pure _) = do
    put (2 :: Word8)


-------------------------------------------------
encodePacket :: (BinaryQ f) =>  f a -> ByteString
encodePacket pkt = runPut (putQ pkt)

decodePacketResult :: (BinaryQ f) => f a -> ByteString -> Either RemoteBinaryException a
decodePacketResult pkt = runGet (interpQ' pkt)

data RemoteBinaryException = RemoteBinaryException String
   deriving (Show, Typeable)

instance Exception RemoteBinaryException


instance Binary RemoteBinaryException where
    put (RemoteBinaryException s) = do put (220:: Word8)
                                       put s

    get = do i <-get
             case i :: Word8 of
               220 -> do s <- get
                         return $ RemoteBinaryException s
               _ -> error "ERROR: unable to parse RemoteBinaryException"
