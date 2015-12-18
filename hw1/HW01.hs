{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------
-- Convert digits in a Number in reverse order
toRevDigits :: Integer -> [Integer]
toRevDigits n  
  |  n < 1 = []
  |  otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : y : xs) = x : (2 * y) : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.

sumi :: [Integer] -> Integer
sumi [] = 0
sumi (x : xs) = x + sumi xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0 
sumDigits (x : xs) = sumDigits xs + (sumi $ toRevDigits x)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits $ doubleEveryOther $ toRevDigits n) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 _ = [(p1, p2)] 
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ 
                (p1, p2) : hanoi (n-1) p3 p2 p1 
