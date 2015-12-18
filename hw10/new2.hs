module New2 where

import Prelude hiding ((>>=), return)
import Control.Monad hiding (return)

newtype Parser a = Parser { run :: String -> Maybe (a,String) }

parse :: Parser a -> String -> Maybe (a, String)
parse p s = run p s 

instance Monad Parser where
return v = Parser $ \s -> Just (v, s)
failure = Parser $ \s -> Nothing
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(>>=) p f = Parser (\str -> case parse p str of
    Nothing -> Nothing
    Just (a, out) -> parse (f a) out)

item :: Parser Char
item = Parser (\inp -> case inp of
               []     -> Nothing 
               (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat p = do 
    x <- item
    if p x then (return x) else failure

main = print "yes"
{-

sat :: (Char -> Bool) -> Parser Char
sat f = item `bind` (\x -> if f x then return' x else failure)


citem = sat (=='c')

pitem :: Parser (Char, Char)
pitem = item `bind` (\x -> item `bind` (\y -> return' (x,y)))

(+++) :: Parser a -> Parser a -> Parser a
(+++) lhs rhs = (\s -> case parse lhs s of
    Nothing -> parse rhs s
    Just sth -> Just sth)

{-
sat f = do 
    x <- item 
    if f x then return' x else failure
-}
-}
    
