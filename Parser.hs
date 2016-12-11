module Parser where

import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \cs -> do 
        (a, as) <- p cs
        return (f a, as)  

instance Applicative Parser where
    pure a = Parser $ \s -> [(a, s)]
    (Parser f) <*> (Parser p) = Parser $ \cs -> do
        (f', cs') <- f cs
        (a, cs'') <- p cs' 
        return (f' a, cs'')
 
instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \cs -> do
        (a, cs') <- p cs
        parse (f a) cs'

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p) <|> (Parser q) = Parser $ \cs -> case p cs of
        [] -> q cs 
        s  -> s

instance MonadPlus Parser where
    mzero = empty
    mplus a b = Parser $ \cs -> parse a cs ++ parse b cs 

item :: Parser Char
item = Parser $ \s -> case s of
    ""     -> []
    (c:cs) -> [(c,cs)] 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else mzero

char :: Char -> Parser Char
char c = satisfy (==c)
