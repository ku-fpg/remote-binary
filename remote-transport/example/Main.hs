{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

{-|
Module:      Control.Remote.Monad.Transport
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Main where


import           Control.Concurrent (forkIO,threadDelay)
import           Control.Remote.Monad.Transport
import           Control.Natural

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import           Network.Transport
import           Network.Transport.TCP

import           System.Environment
import           System.IO

main :: IO ()
main = getArgs >>= main2

main2 [] = do
  hSetBuffering stdout LineBuffering 
  hSetBuffering stderr LineBuffering 
  forkIO $ main2 ["server"]
  threadDelay $ 1000 * 1000
  main2 ["client"]

main2 ("client":_) = do
  Right transport <- createTransport "localhost" "30178" defaultTCPParameters
  Nat f <- transportClient transport $ encodeEndPointAddress "localhost" "30179" 0
  print "calling"
  bs <- f $ Sync $ LBS.pack [1,2,3,4]
  print "replied"
  print bs
  print "done"
--  transportClient transport "localhost:30178"
  return ()  

main2 ("server":_) = do 
  Right transport <- createTransport "localhost" "30179" defaultTCPParameters
  transportServer transport $ Nat $ \ (Sync bs) -> do
      print ("Remote BS:"::String,bs)
      return $ LBS.pack [7,8,9]
    
  return ()
  
main2 _ = return ()
