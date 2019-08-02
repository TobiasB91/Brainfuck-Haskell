module Statement where

data Statement = MoveL | MoveR | Increment | Decrement | Output | Input | Loop [Statement] | EOF | Skip deriving (Show, Eq)
