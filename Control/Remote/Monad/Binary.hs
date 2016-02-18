{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

{-|
Module:      Control.Remote.Monad.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Binary (




) where
import           Control.Monad (void)
import           Control.Natural
import qualified Control.Remote.Monad.Packet.Weak as WP
import           Control.Remote.Monad.Binary.Types
import           Data.Binary
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS


sendWeakBinary :: (SendAPI ~> IO) -> WP.WeakPacket Command Procedure a -> IO a
--sendWeakBinary f pkt@(WP.Command c)   = do r <- f (Sync (encode (T pkt))) 
--                                           return ()
sendWeakBinary f pkt = do 
                        (Just r) <- f (Sync (runPut $ encodeWeakPacket  pkt))
                        return $ decodeWeakPacketResult pkt r 
                                             




runWeakBinary :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO 
runWeakBinary = BinaryNatTrans  dispatchPacket

receiveSendAPI :: BinaryNatTrans (WP.WeakPacket Command Procedure)  IO -> (SendAPI ~> IO)
receiveSendAPI (BinaryNatTrans f) (Sync c) = do 
                     print c
                     case decode c of 
                       (T v{-@(WP.Command _c)-}) -> do  
                                     r <- f v
                                     return $ Just $ encodeWeakPacketResult v r
{-
receiveSendAPI (BinaryNatTrans f) (Sync c) = do 
                     print c
                     case decode c of
                       (T c') ->do  
                                   v <- f c'
                                   print v
                                   return $ encode v
-}
dispatchPacket :: WP.WeakPacket Command Procedure a -> IO a
dispatchPacket (WP.Command (Command n)) = print $ "Push "++ (show n)  
dispatchPacket (WP.Procedure (Procedure)) = do print $ "Pop"  
                                               return (5 ::Int)

main::IO()
main = do 
        let f1 = receiveSendAPI runWeakBinary
        r <- sendWeakBinary f1 (WP.Procedure (Procedure))
        print r
        return ()
