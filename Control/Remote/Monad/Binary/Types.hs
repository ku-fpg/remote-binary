{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

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
 , RemotePacket(..)
 , SendAPI(..)
 )
 where

import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)


-------------

data SendAPI :: * -> * where
    Sync  :: ByteString -> SendAPI ByteString

data RemotePacket (p :: * -> *) where
   RemotePacket :: (Binary a) => p a -> RemotePacket p

class BinaryQ p  where
  putQ    :: p a -> Put            -- ^ encode a query/question as a packet
  getQ    :: Get (RemotePacket p)  -- ^ decode to the packet, which contains a way of encoding the answer
  interpQ :: p a -> Get a          -- ^ interprete the answer, in the context of the original query (type).

-------------
-- ############## Weak Packet BinaryQ Instance #######################

instance (Binary c, BinaryQ p) => BinaryQ (WP.WeakPacket c p) where
  putQ = putWeakPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do 
                c <- get
                return $ RemotePacket $ WP.Command c
          1 -> do 
                RemotePacket p <- getQ
                return $ RemotePacket $ WP.Procedure $ p

  interpQ (WP.Command c)   = return ()
  interpQ (WP.Procedure p) = interpQ p


putWeakPacket :: (Binary c, BinaryQ p) => WP.WeakPacket c p a -> Put
putWeakPacket (WP.Command c) = do
   put (0 :: Word8)
   put c
putWeakPacket (WP.Procedure p)= do
   put (1 :: Word8)
   putQ p


---- ############ Strong Packet BinaryQ Instance ##########################

instance (Binary c, BinaryQ p) => BinaryQ (SP.StrongPacket c p) where
  putQ = putStrongPacket

  getQ = do
       i <- get
       case i :: Word8 of
          0 -> do 
                c <- get
                RemotePacket cmds <- getQ
                return $ RemotePacket $ SP.Command c cmds
          1 -> do 
                RemotePacket p <- getQ
                return $ RemotePacket $ SP.Procedure p
          2 -> do
                return $ RemotePacket $ SP.Done

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

---- ############ Applicative Packet BinaryQ Instance #####################
instance (Binary c, BinaryQ p) => BinaryQ (AP.ApplicativePacket c p) where
  putQ = putApplicativePacket 

  getQ = do
      i <- get
      case i :: Word8 of
         0 -> do
              c <- get
              return $ RemotePacket $ AP.Command c
         1 ->do 
                RemotePacket p <- getQ
                return $ RemotePacket $ AP.Procedure $ p

         2 -> do RemotePacket q1 <- getQ
                 RemotePacket q2 <- getQ
                 return $ RemotePacket $ AP.Zip (\ a b -> (a,b)) q1 q2
         3 -> return $ RemotePacket $ AP.Pure ()


  interpQ (AP.Command c)   = return ()
  interpQ (AP.Procedure p) = interpQ p
  interpQ (AP.Zip f x y)   = f <$> interpQ x <*> interpQ y
  interpQ (AP.Pure a)      = return a


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


-------------------------------------------------
encodePacket :: (BinaryQ f) =>  f a -> ByteString 
encodePacket pkt = runPut (putQ pkt)

decodePacketResult :: (BinaryQ f) => f a -> ByteString -> a
decodePacketResult pkt = runGet (interpQ pkt)
