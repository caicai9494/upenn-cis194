-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------
testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (i, lst) = toRevDigits i == lst

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3,2,1]), (1234, [4,3,2,1]), (5, [5]), (10, [0, 1]), (0, []), (-1, [])]
           ]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (lhs, rhs) = doubleEveryOther lhs == rhs

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([3,2,1], [3,4,1]), ([4,3,2,1], [4,6,2,2]), ([5], [5]), 
              ([0, 1], [0, 2]), ([], [])]
           ]

-- Exercise 4 -----------------------------------------
testSumi :: ([Integer], Integer) -> Bool
testSumi (xs, s) = sumi xs == s

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, s) = sumDigits xs == s

ex4Tests :: [Test]
ex4Tests = [ Test "sumi test" testSumi
             [([3,2,1], 6), ([4,3,2,1], 10), ([5], 5), 
              ([0, 1], 1), ([], 0)],
             Test "sumDigits test" testSumDigits
             [([3,2,1], 6), ([4,3,2,1], 10), ([5], 5), 
              ([0, 1], 1), ([], 0), ([10, 5, 18, 4], 19)]
           ]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (i, b) = luhn i == b
ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), 
              (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------
testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, a, b, c, m) = (hanoi n a b c) == m
ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")])] 
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
