{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Types where
 
import           Control.Remote.Monad
import           Control.Remote.Monad.Binary
import           Data.Binary

{-
import           Control.Remote.Monad.Transport
import           Network.Transport  hiding (send)
import           Network.Transport.TCP
-}


data Command :: * where
   Push :: Int -> Command   -- PUSH
 deriving (Show)

instance Binary Command where
   put (Push n) = put n
   get = do i <- get
            return $ Push i

data Procedure :: * -> * where
  Pop :: Procedure (Either String Int)    -- POP

instance BinaryQ Procedure where
   getQ = do i <- get
             case i :: Word8 of
               0 -> return $ Fmap put Pop
   putQ (Pop)= put (0 :: Word8) 

   interpQ (Pop) = get





push :: Int -> RemoteMonad Command Procedure ()
push n = command $ Push n

pop :: RemoteMonad Command Procedure (Either String Int)
pop = procedure $ Pop
