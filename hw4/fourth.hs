import System.IO

class (Lin a)  where
    cool :: a -> String
    yesno :: a -> Bool

data MyInt = MyInt Int

data MyAbs a = Low | High a
    deriving (Show)

instance Monad MyAbs where
    (>>=) Low _ = Low 
    (>>=) (High h) f = f h 

    return n = High n

    (>>) _ b = b
someFoo :: MyAbs Int 
someFoo = High 5 >>= (*5)

instance Functor MyAbs where
    fmap f Low = Low 
    fmap f (High x) = High $ f x 

instance Eq MyInt where 
    --MyInt a == MyInt b = a == b
    (==) (MyInt a) (MyInt b) = a == b
    x /= y = not (x == y)

instance Num MyInt where 
    (MyInt a) + (MyInt b) = MyInt (a + b)
    (MyInt a) - (MyInt b) = MyInt (a - b)
    (MyInt a) * (MyInt b) = MyInt (a * b)
    abs (MyInt a) = MyInt (abs a)

instance Show MyInt where 
    show (MyInt a) = "MyInt " ++ (show a)

instance Lin MyInt where
    cool _ = "Great!"
    yesno (MyInt 0) = False
    yesno _ = True

hola :: (Num a) => a -> String
hola _ = "yes"

data CannotShow = CannotShow
    deriving (Show)

data Option a b = Option Int | Int a
    deriving (Show)

data MyList a = MyList [a] | Empty
    deriving (Show)
foo :: MyList Bool -> String
foo _ = "Cool"

{-
instance Functor IO where
    fmap f action  = do
        line <- action
        return $ f line
 -}       

main :: IO ()
main = do
    line <- fmap reverse getLine
    putStrLn line

    line <- fmap (++"!") getLine
    putStrLn line

