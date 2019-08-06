module BFParser (parseStmts, Statement(..)) where

import Control.Monad
import Control.Monad.Free
import Data.Functor
import Control.Applicative
import Parser
import Statement (Statement(..))
import qualified Statement as S

moveL, moveR, increment, decrement, output, 
  input, loop, skip, eof, stmt, stmts :: Parser (Free Statement ()) 

moveL = char '<' >> return S.moveL

moveR = char '>' >> return S.moveR

increment = char '+' >> return S.increment

decrement = char '-' >> return S.decrement

output = char '.' >> return S.output

input = char ',' >> return S.input 

loop = do
  char '[' 
  s <- foldr (>>) (Pure ()) <$> many stmt
  char ']'
  return $ S.loop s 
      
skip = satisfy (not . flip elem "[]") >> return S.skip

eof = return S.eof

stmt = moveR <|> moveL <|> increment <|> decrement 
    <|> output <|> input <|> loop <|> skip  

stmts = do
  s <- stmt <|> eof 
  case s of 
    Pure () -> return S.eof 
    act     -> (act >>) <$> stmts 

parseStmts :: String -> Free Statement ()
parseStmts inp = case parse stmts inp of
    [(st, "")] -> st
    [(st, r)]  -> error $ "Parse error on input '" ++ [head r] 
        ++ "' (" ++ show (length inp - length r) ++ ")"

