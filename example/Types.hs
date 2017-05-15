{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
module Types where

import           Control.Remote.Monad
import           Control.Remote.Monad.Binary
import           Data.Binary

data Prim :: * -> * where
  Push :: Int -> Prim ()   -- PUSH
  Pop  ::        Prim Int  -- POP

instance KnownResult Prim where
  knownResult (Push _) = Just ()
  knownResult  Pop     = Nothing

instance BinaryQ Prim where
   getQ = do i <- get
             case i :: Word8 of
               0 -> do
                    j <- get
                    return $ Fmap put (Push j)
               1 -> return $ Fmap put Pop
               _ -> error "Expected a Prim but got something else"
   putQ (Push i) =do
                     put (0 :: Word8)
                     put i
   putQ (Pop)  = put (1 :: Word8)

   interpQ (Push _) = return ()
   interpQ (Pop)  = get


push :: Int -> RemoteMonad Prim ()
push n = primitive $ Push n

pop :: RemoteMonad Prim Int
pop = primitive $ Pop
