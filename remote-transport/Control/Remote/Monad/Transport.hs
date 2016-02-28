{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

{-|
Module:      Control.Remote.Monad.Transport
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Transport  where

import           Control.Remote.Monad.Binary.Types (SendAPI(..))
import           Control.Natural
import           Data.Binary
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS


transportClient :: Transport -> IO (SendAPI :~> IO)
transportClient = undefined


transportServer :: Transport -> (SendAPI :~> IO) -> IO ()
transportServer = undefined
