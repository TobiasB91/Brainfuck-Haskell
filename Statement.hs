{-# LANGUAGE DeriveFunctor #-}

module Statement where

import Control.Monad.Free

data Statement a = 
    MoveL a
  | MoveR a
  | Increment a
  | Decrement a
  | Output a
  | Input a
  | Loop a a 
  | Skip a 
  | EOF deriving Functor 


moveL :: Free Statement () 
moveL = liftF (MoveL ())

moveR :: Free Statement () 
moveR = liftF (MoveR ())

increment :: Free Statement () 
increment = liftF (Increment ())

decrement :: Free Statement () 
decrement = liftF (Decrement ())

output :: Free Statement () 
output = liftF (Output ())

input :: Free Statement () 
input = liftF (Input ())

loop :: Free Statement () 
loop = undefined 

skip :: Free Statement ()
skip = liftF (Skip ())

eof :: Free Statement r
eof = Free EOF
