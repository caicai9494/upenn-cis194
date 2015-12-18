{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List

-- ex1 
-- Implement slow fib
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fibs1 :: [Integer]
fibs1 = map fib [0..]

-- ex2 
fib2 :: Integer -> Integer
fib2 n = helper 0 1 n
    where helper :: Integer -> Integer -> Integer -> Integer
	  helper a _ 0 = a 
	  helper _ b 1 = b 
	  helper a b n = helper b (a + b) (n - 1) 

fibs2 :: [Integer]
fibs2 = map fib2 [0..]


--data MyList a = MyEmpty | MyCons a (MyList a) 
--    deriving (Show)

-- ex 3
data Stream a = Cons a (Stream a) 

streamToList :: Show a => Stream a -> [a]
streamToList (Cons x stm)  = x : (streamToList stm)

instance (Show a) => Show (Stream a) where
    show stm = show $ take showLen (streamToList stm) where
        showLen = 200

foo :: Stream Int
foo = Cons 1 $ Cons 2 $ Cons 3 foo 


-- ex 4
streamRepeat :: Show a => a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (Show a, Show b) => (a -> b) -> Stream a -> Stream b
streamMap f (Cons x stm) = Cons itr $ streamMap f stm where
    itr = f x
    

streamFromSeed :: Show a => (a -> a) -> a -> Stream a 
streamFromSeed f x = Cons itr $ streamFromSeed f itr where 
    itr = f x

-- ex 5
nats :: Stream Integer 
nats = streamFromSeed (+1) (- 1) 

positives :: Stream Integer
positives = streamFromSeed (+1) 0 

interleaveStream :: Show a => Stream a -> Stream a -> Stream a
interleaveStream (Cons x stmx) (Cons y stmy) = Cons x (Cons y (interleaveStream stmx stmy))

zeros :: Stream Integer
zeros = streamRepeat 0

ruler :: Stream Integer
ruler = streamMap divMax positives

divMax :: Integer -> Integer
divMax n 
   | n `mod` 2 /= 0 = 0
   | n == 0 = 0
   | otherwise = 1 + divMax (n `div` 2)

-- ex6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0 

multBy :: (Show a, Num a) => a -> Stream a -> Stream a
multBy i = streamMap (\x -> x * i)  

multByX :: (Show a, Num a) => Stream a -> Stream a
multByX stm = Cons (fromInteger 0) stm  

instance (Show a, Num a) => Num (Stream a) where
    fromInteger n = Cons (fromInteger n) $ streamRepeat 0
    negate = streamMap (0-)
    (Cons l lsm) + (Cons r rsm) = Cons (l + r) (lsm + rsm)   
    (Cons a0 asm) * (Cons b0 bsm) = Cons (a0 * b0) $ (streamRepeat 0)  + (multBy a0 bsm + (asm * (Cons b0 bsm)))   
    
--instance (Show a, Num a, Fractional a) => Fractional (Stream a) where
--    (Cons a0 asm) / (Cons b0 bsm) = Cons (a0 / b0) $ (streamRepeat 0)  + (1 / bo) * (multBy a0 bsm + (asm * (Cons b0 bsm)))   
    
--2015 ex7
sTake :: Show a => Integer -> Stream a -> [a]
sTake 0 (Cons x _) = [x]   
sTake n (Cons x stm) = x : (sTake (n - 1) stm)   

rand :: Integer -> Stream Integer
rand = streamFromSeed (\a -> (1103515245 * a + 12345) `mod` 2147483648) 

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Integer] -> Maybe (Integer, Integer)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Integer] -> Maybe (Integer, Integer)
minMax [] = Nothing   -- no min or max if there are no elements
minMax (x:xs) = Just $ go x x xs where
    go xmin xmax [] = (xmin, xmax)
    go !xmin !xmax (y:ys) = go (min y xmin) (max y xmax) ys

{- Total Memory in use: 1 MB -}
minMax2 :: [Integer] -> Maybe (Integer, Integer)
minMax2 [] = Nothing   -- no min or max if there are no elements
minMax2 (x:xs) = Just $ foldl' (\ (!xmin, !xmax) x -> (min xmin x, max xmax x)) (x, x) xs

main :: IO ()
--main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
