module Main where

import BFParser
import Statement
import Data.Char
import System.IO
import System.Environment

data Machine = M [Int] Int [Int] deriving Show

start :: Machine 
start = M [] 0 [] 

eval :: [Statement] -> Machine -> IO Machine
eval (MoveL:st) (M l x r) = case l of 
    []      -> eval st $ M l 0 (x:r)  
    (l':ls) -> eval st $ M ls l' (x:r) 
eval (MoveR:st) (M l x r) = case r of
    []      -> eval st $ M (x:l) 0 r
    (r':rs) -> eval st $ M (x:l) r' rs
eval ((Loop xs):st) (M l x r) = case x of
    0 -> eval st $ M l x r
    _ -> eval xs (M l x r) >>= eval ((Loop xs):st)
eval (Increment:st) (M l x r) = eval st $ M l (x+1) r
eval (Decrement:st) (M l x r) = eval st $ M l (x-1) r
eval (Output:st) (M l x r) = (putStr . (:[]) . chr) x >> eval st (M l x r)
eval (Input:st) (M l _ r) = getChar >>= \c -> eval st (M l (ord c) r) >> return (M l (ord c) r)
eval (Skip:st) m = eval st m
eval x m = return m 

main :: IO () 
main = do
    [name] <- getArgs
    contents <- readFile name
    eval (parseStmts contents) start 
    return ()
