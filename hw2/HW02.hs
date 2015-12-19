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
exactMatches lhs rhs = length $ filter (uncurry (==)) $ zip lhs rhs 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = go colors c
    where go :: Code -> Code -> [Int]
          go _ [] = []
          go [] _ = []
          go (p:ps) ts = (length $ filter (==p) ts) : go ps ts  

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches lhs rhs = sum $ zipWith min (countColors lhs) (countColors rhs) 

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
filterCodes m = filter (isConsistent m)  

-- Exercise 6 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = let code = allCodes (n - 1) in
             concat $ map (\c -> map (c:) code) colors

-- use list comprehension
allCodesl :: Int -> [Code]
allCodesl 0 = [[]]
allCodesl n = [c : code | c <- colors, code <- allCodesl (n-1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = let len = length secret 
                   brutal = allCodes len
               in filter (\(Move _ i _) -> i == len) $ map (getMove secret) brutal

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
