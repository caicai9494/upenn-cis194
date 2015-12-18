{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (liftM)
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

roll :: Int -> Rand StdGen [DieValue]
roll n | n < 1 = fmap (:[]) die
       | otherwise = do 
	    i <- die 
	    is <- roll (n-1)
	    return (i:is)
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

sortRev :: [Int] -> [Int]
sortRev = sortBy (flip compare) 

mock :: [Int] -> [Int] -> (Int, Int)
mock lhs rhs = foldr helper (lenlhs, lenrhs) $ zip (sortRev lhs) (sortRev rhs) where
    helper = (\(l,r) (al, ar) -> if (l > r) then (al, ar-1) else (al-1, ar))
    lenlhs = length lhs
    lenrhs = length rhs

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do 
    attk <- roll $ attackers bf
    defd <- roll $ defenders bf
    return $ uncurry Battlefield $ mock (liftM unDV attk) (liftM unDV defd)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    bf'@ Battlefield { attackers = a, defenders = d } <- battle bf
    if d == 0 || a < 2 then return bf' else invade bf'

successProb :: Battlefield -> Rand StdGen Double
successProb bf = simulate 1000 bf (0,0) >>= (\(a,b) -> return $ fromIntegral a / fromIntegral (a+b)) where

    simulate :: Int -> Battlefield -> (Int, Int) -> Rand StdGen (Int, Int)
    simulate n bf (attk,defd) 
        | n == 0 = return (attk,defd)
        | otherwise = do 
	    bf' <- invade bf
	    if defwin bf' then simulate (n-1) bf (attk, defd+1)
	                  else simulate (n-1) bf (attk+1, defd)

    defwin :: Battlefield -> Bool
    defwin Battlefield {attackers = a, defenders = d} = d == 0
