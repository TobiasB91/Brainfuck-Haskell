{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}

module Statement where

import Control.Monad
import Control.Monad.Free

data Statement a = 
    MoveL a
  | MoveR a
  | Increment a
  | Decrement a
  | Output a
  | Input a
  | Loop (Free Statement ()) a
  | Skip a deriving Functor 

moveL, moveR, increment, decrement, 
  output, input, skip, eof  :: Free Statement () 

moveL = liftF (MoveL ())

moveR = liftF (MoveR ())

increment = liftF (Increment ())

decrement = liftF (Decrement ())

output = liftF (Output ())

input = liftF (Input ())

loop :: Free Statement () -> Free Statement ()
loop bdy = liftF $ Loop bdy ()

skip = liftF (Skip ())

eof = Pure ()  
