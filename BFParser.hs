module BFParser (parseStmts, Statement(..)) where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Parser
import Statement (Statement(..))
import qualified Statement as S


moveL :: Parser (Free Statement ())
moveL = char '<' >> return S.moveL

moveR :: Parser (Free Statement ())
moveR = char '>' >> return S.moveR

increment :: Parser (Free Statement ())
increment = char '+' >> return S.increment

decrement :: Parser (Free Statement ())
decrement = char '-' >> return S.decrement

output :: Parser (Free Statement ())
output = char '.' >> return S.output

input :: Parser (Free Statement ())
input = char ',' >> return S.input 

loop :: Parser (Free Statement ())
loop = do
  char '[' 
  s <- stmts
  char ']'
  return $ Free (Loop 
      

skip :: Parser (Free Statement ())
skip = satisfy (not . flip elem "[]") >> return S.skip

eof :: Parser (Free Statement ())
eof = return S.eof

stmt :: Parser (Free Statement ()) 
stmt = moveR <|> moveL <|> increment <|> decrement 
    <|> output <|> input <|> loop <|> skip  

stmts :: Parser (Free Statement ()) 
stmts = do
  s <- stmt <|> eof 
  case s of 
    Free EOF -> return $ Free EOF
    act      -> (act >>) <$> stmts 

{-
parseStmts :: String -> [Statement]
parseStmts inp = case parse stmts inp of
    [(st, "")] -> st
    [(st, r)]  -> error $ "Parse error on input '" ++ [head r] 
        ++ "' (" ++ show (length inp - length r) ++ ")"

-}
