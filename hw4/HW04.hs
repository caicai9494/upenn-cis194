{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------


instance (Num a, Eq a) => Eq (Poly a) where
    P a == P b = toProper a == toProper b
	where toProper :: (Num a, Eq a) => [a] -> [a]
	      toProper [] = []
	      toProper xs = if last xs /= 0 then xs else toProper $ init xs
--idk :: (Num a, Eq a) => [a]
--idk = [1]

-- TODO 
{-
instance (Num a, Eq a) => Eq (Poly a) where
    P lhs == P rhs = if l1 /= l2 then False else foldr (\(x,y) acc -> acc && x==y) True (zip plhs prhs) 
	where process :: (Num a, Eq a) => [a] -> [a]
	      process xs = reverse $ dropWhile (\x -> x == fromInteger 0) $ reverse xs 
	      plhs, prhs :: (Num a, Eq a) => [a]
	      plhs = process lhs
	      prhs = process rhs
	      l1, l2 :: Int
	      l1 = length plhs
	      l2 = length prhs
-}
-- Exercise 3 -----------------------------------------
{-
properCoeff :: (Num a, Eq a, Show a) => a -> Int -> String
properCoeff 0 _ = ""
properCoeff c 0 = show c ++ "x"
properCoeff c 1 = show c ++ "x" ++ show 1
properCoeff c e = show c ++ "x^" ++ show e
-}

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) = convert xs 0
        where convert :: (Num a, Eq a, Show a) => [a] -> Int -> String
              convert [] _ = ""
              convert (y:[]) i = (show y) ++ "x^" ++ (show i)
              convert (y:ys) i = convert ys (i + 1) ++ " + " ++ (show y) ++ "x^" ++ (show i)  

        

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P lhs) (P rhs) = P $ add lhs rhs
    where add :: Num a => [a] -> [a] -> [a]
          add [] rx = rx
          add lx [] = lx
          add (l:ls) (r:rs) = (l + r) : add ls rs 

-- Exercise 5 -----------------------------------------

multiplyBy :: Num a => Poly a -> a -> Poly a
multiplyBy (P ys) n = P $ map (\y -> y * n) ys

multiplyBy10 :: Num a => Poly a -> Poly a
multiplyBy10 (P ys) = P (0:ys)

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) (P _) = P []
times (P (p:p1)) p2 = let multP = multiplyBy p2 p 
                          multRest = times (P p1) (multiplyBy10 p2)
                      in plus multP multRest
-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P $ map (\y -> -y) xs  
    fromInteger n = P $ [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P ys) n = foldr (\y acc -> acc * n + y) 0 ys

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = deriv (nderiv (n - 1) f) 

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ys)) = P (zipWith (*) ys [1..])

