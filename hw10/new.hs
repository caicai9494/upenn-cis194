
type Parser a = String -> [(a,String)]

instance Monad Parser where
    return v = \s -> [(v, s)]
    --fail = \s -> []


item :: Parser Char
item = \inp -> case inp of
               []     -> []
               (x:xs) -> [(x,xs)] 

