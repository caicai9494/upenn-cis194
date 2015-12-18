module Golf where

import Data.List

skipn :: [a] -> Int -> [a]
skipn xs n = reverse $ snd $ foldl (\(c,ys) y -> if n == c then (1, y:ys) else (c + 1, ys)) (n,[]) xs

skips :: [a] -> [[a]]
skips lst = let l = length lst in foldr (\x acc -> (skipn (drop (x - 1) lst) x):acc) [] [1..l]


localMaxima :: [Int] -> [Int]
localMaxima (a:b:c:xs) = if b > a && b > c then (b:(localMaxima (c:xs))) else (localMaxima (b:c:xs))
localMaxima _ = []
{-
type Dict = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

zero :: Dict
zero = (0,0,0,0,0,0,0,0,0,0,0)

counting :: [int] -> Dict -> Dict
counting [] d = d
counting (0:xs) (a,_,_,_,_,_,_,_,_,_) = (a+1,_,_,_,_,_,_,_,_,_)
counting (1:xs) (_,a,_,_,_,_,_,_,_,_) = (_,a+1,_,_,_,_,_,_,_,_)
counting (2:xs) (_,_,a,_,_,_,_,_,_,_) = (_,_,a+1,_,_,_,_,_,_,_)
counting (3:xs) (_,_,_,a,_,_,_,_,_,_) = (_,_,_,a+1,_,_,_,_,_,_)
counting (4:xs) (_,_,_,_,a,_,_,_,_,_) = (_,_,_,_,a+1,_,_,_,_,_)
counting (5:xs) (_,_,_,_,_,a,_,_,_,_) = (_,_,_,_,_,a+1,_,_,_,_)
counting (6:xs) (_,_,_,_,_,_,a,_,_,_) = (_,_,_,_,_,_,a+1,_,_,_)
counting (7:xs) (_,_,_,_,_,_,_,a,_,_) = (_,_,_,_,_,_,_,a+1,_,_)
counting (8:xs) (_,_,_,_,_,_,_,_,a,_) = (_,_,_,_,_,_,_,_,a+1,_)
counting (9:xs) (_,_,_,_,_,_,_,_,_,a) = (_,_,_,_,_,_,_,_,_,a+1)

histogram :: [Int] -> String
histogram lst = counting lst zero 
    where counting :: [int] -> Dict -> Dict
          counting [] d = d
          counting (0:xs) (a,_,_,_,_,_,_,_,_,_) = (a+1,_,_,_,_,_,_,_,_,_)
          counting (1:xs) (_,a,_,_,_,_,_,_,_,_) = (_,a+1,_,_,_,_,_,_,_,_)
          counting (2:xs) (_,_,a,_,_,_,_,_,_,_) = (_,_,a+1,_,_,_,_,_,_,_)
          counting (3:xs) (_,_,_,a,_,_,_,_,_,_) = (_,_,_,a+1,_,_,_,_,_,_)
          counting (4:xs) (_,_,_,_,a,_,_,_,_,_) = (_,_,_,_,a+1,_,_,_,_,_)
          counting (5:xs) (_,_,_,_,_,a,_,_,_,_) = (_,_,_,_,_,a+1,_,_,_,_)
          counting (6:xs) (_,_,_,_,_,_,a,_,_,_) = (_,_,_,_,_,_,a+1,_,_,_)
          counting (7:xs) (_,_,_,_,_,_,_,a,_,_) = (_,_,_,_,_,_,_,a+1,_,_)
          counting (8:xs) (_,_,_,_,_,_,_,_,a,_) = (_,_,_,_,_,_,_,_,a+1,_)
	  counting (9:xs) (_,_,_,_,_,_,_,_,_,a) = (_,_,_,_,_,_,_,_,_,a+1)
-}
