{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

codes :: [Code]
codes = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches lhs rhs = length $ filter (\(x,y) -> x == y) $ zip lhs rhs 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = go colors c
    where go :: Code -> Code -> [Int]
          go _ [] = []
          go [] _ = []
          go (p:ps) ts = (length $ filter (\x -> x == p) ts) : go ps ts  

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches lhs rhs = sum $ map (\(x,y) -> min x y) $ zip (countColors lhs) (countColors rhs) 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove lhs rhs = Move rhs rightCount (matchesCount - rightCount)
    where rightCount, matchesCount :: Int
          rightCount = exactMatches lhs rhs 
          matchesCount = matches lhs rhs
            

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move lhs a b) rhs = case (getMove lhs rhs) of
    (Move _ a' b') -> a' == a && b' == b
                                       

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m c = filter (isConsistent m) c  

-- Exercise 6 -----------------------------------------
--temp = foldl (\acc x -> [x] ++ acc) [[Red]] colors  
{-
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

codes :: [Code]
codes = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
-}
--temp2 :: [Code] -> [Code]
--temp2 codes = map (\x -> foldl (\acc y -> y : acc) [x] colors) colors --[[Red]]
allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [ c : code | c <- colors, code <- allCodes (n - 1) ]
{-
allCodes :: Int -> [Code]
allCodes len = helper len codes
    where helper :: Int -> [Code] -> [Code]
          helper 1 acc = acc 
          helper n acc = helper (n-1) (map (\x -> map (\y -> x ++ [y]) colors) acc)
-}
-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
