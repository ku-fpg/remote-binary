{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Original where
 
import Control.Monad.State

data Command :: * where
   Push :: Int -> Command   -- PUSH
 deriving (Show)

data Procedure :: * -> * where
  Pop :: Procedure Int    -- POP


evalProcedure :: Procedure a-> State [Int] a
evalProcedure (Pop) = do st <- get
                         case st of
                           []  -> error "Can't pop an empty stack"
                           (x:xs) -> do 
                                      put xs
                                      return x

evalCommand :: Command -> State [Int] ()
evalCommand (Push n) = modify (n:)


main:: IO()
main = do 
       let (a,s) = runState (evalCommand (Push 3)) [1,2,3,4] 
       print a 
       print s

       return ()
