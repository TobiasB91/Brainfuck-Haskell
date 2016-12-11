module BFParser (parseStmts, Statement(..)) where

import Control.Monad
import Control.Applicative
import Parser

data Statement = MoveL | MoveR | Increment | Decrement | Output | Input | Loop [Statement] | EOF | Skip deriving (Show, Eq)

moveL :: Parser Statement
moveL = char '<' >> return MoveL

moveR :: Parser Statement
moveR = char '>' >> return MoveR

increment :: Parser Statement
increment = char '+' >> return Increment

decrement :: Parser Statement
decrement = char '-' >> return Decrement

output :: Parser Statement
output = char '.' >> return Output

input :: Parser Statement
input = char ',' >> return Input 

loop :: Parser Statement
loop = do
    char '[' 
    xs <- many stmt
    char ']'
    return $ Loop xs

skip :: Parser Statement
skip = satisfy (not . flip elem "[]") >> return Skip

eof :: Parser Statement
eof = return EOF

stmt :: Parser Statement 
stmt = moveR <|> moveL <|> increment <|> decrement 
    <|> output <|> input <|> loop <|> skip  

stmts :: Parser [Statement]
stmts = do
    s <- stmt <|> eof
    case s of
        EOF  -> return [EOF]
        _    -> stmts >>= \xs -> return $ s:xs

parseStmts :: String -> [Statement]
parseStmts inp = case parse stmts inp of
    [(st, "")] -> st
    [(st, r)]  -> error $ "Parse error on input '" ++ [head r] 
        ++ "' (" ++ show (length inp - length r) ++ ")"
