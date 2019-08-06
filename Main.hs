{-# LANGUAGE LambdaCase #-}
module Main where

import BFParser (parseStmts)
import Statement
import Data.Char
import System.IO
import System.Environment

import Data.Functor
import Control.Monad
import Control.Monad.Free

import Control.Monad.State.Lazy

data Machine = M {left :: [Int], ptr :: Int, right :: [Int]} deriving Show

start :: Machine 
start = M [] 0 [] 

eval :: Free Statement () -> IO ()
eval = flip evalStateT start . eval' 

incr, decr, movL, movR :: StateT Machine IO ()
incr = modify $ \(M l x r) -> M l (x+1) r
decr = modify $ \(M l x r) -> M l (x-1) r
movL = modify $ \case 
  M [] x r -> M [] 0 (x:r)
  M (l':ls) x r -> M ls l' (x:r)
movR = modify $ \case 
  M l x [] -> M (x:l) 0 []
  M l x (r':rs) -> M (x:l) r' rs

eval' :: Free Statement () -> StateT Machine IO ()
eval' (Free (MoveL next)) = movL >> eval' next
eval' (Free (MoveR next)) = movR >> eval' next 
eval' l@(Free (Loop body next)) = get >>= \case
  M _ 0 _ -> eval' next
  _       -> eval' body >> eval' l 
eval' (Free (Increment next)) = incr >> eval' next
eval' (Free (Decrement next)) = decr >> eval' next
eval' (Free (Output next)) = (get >>= lift . putStr . (:[]) . chr . ptr) >> eval' next
eval' (Free (Input next)) = do 
  c <- lift getChar
  M l _ r <- get
  put $ M l (ord c) r
  eval' next
eval' (Free (Skip next)) = eval' next
eval' (Pure ()) = pure ()

prettyPrinter :: Free Statement () -> IO () 
prettyPrinter = flip evalStateT 0 . prettyPrinter'


prettyPrinter' :: Free Statement () -> StateT Int IO ()
prettyPrinter' (Free (MoveL next)) = printCmd "MoveLeft;" >> prettyPrinter' next
prettyPrinter' (Free (MoveR next)) = printCmd "MoveRight;" >> prettyPrinter' next
prettyPrinter' l@(Free (Loop body next)) = printCmd "Loop" >> modify (+1) >> prettyPrinter' body >> modify (+(-1)) >> prettyPrinter' next 
prettyPrinter' (Free (Increment next)) = printCmd "Increment;" >> prettyPrinter' next
prettyPrinter' (Free (Decrement next)) = printCmd "Decrement;" >> prettyPrinter' next
prettyPrinter' (Free (Output next)) = printCmd "Output;" >> prettyPrinter' next
prettyPrinter' (Free (Input next)) = printCmd "Input;" >> prettyPrinter' next
prettyPrinter' (Free (Skip next)) = prettyPrinter' next
prettyPrinter' (Pure ()) = pure () 
  
printCmd :: String -> StateT Int IO ()
printCmd cmd = do 
  i <- get
  lift $ putStrLn $ replicate (i*2) ' ' ++ cmd

test = do
  replicateM_ 99 increment 
  output
  loop (do
    decrement
    moveR
    increment
    moveL)
  output


main :: IO () 
main = do
  [name] <- getArgs
  contents <- readFile name
  prettyPrinter $ parseStmts contents
  eval $ parseStmts contents
