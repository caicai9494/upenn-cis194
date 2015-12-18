module JoinList where

import Data.Monoid
import Data.Maybe
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- ex 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
(+++) lhs rhs = Append v lhs rhs
    where v = tag lhs <> tag rhs

{-
foldrsJ :: (Sized m, Monoid m) => (JoinList m a -> Int -> c) -> -- when index invalid f
                                  (JoinList m a -> Int -> c) -> -- when empty g
                                  (JoinList m a -> Int -> c) -> -- when single h
                                  (JoinList m a -> JoinList m a -> Int -> c) -> -- when append k
                                  Int -> -- index
                                  JoinList b a   
                                  c -> -- accu
foldrsj f _ _ _ i _ | i < 0 = f i
foldrsj _ g _ _ i Empty = g i
foldrsj _ _ h _ i s@(Single m a) = h s i 
foldrsj _ _ _ k i (Append m lhs rhs) = Append m
-}

-- ex 2
getSizeJ :: Monoid m => JoinList m a -> Int 
getSizeJ l = getSize $ size $ tag l

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single b a)  
    | (getSize $ size b) == i = Just a 
    | otherwise = Nothing    
indexJ i (Append b lhs rhs)  
    | (getSize $ size b) > i = Nothing 
    | otherwise = if (getSizeJ lhs) < i then indexJ i rhs
                                                    else indexJ i lhs

-- ex 3
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i _ | i < 0 = Empty
dropJ i s@(Single b a) = if (getSize $ size b) == i then Empty else s 
dropJ i (Append b lhs rhs) = if (getSize $ size b) == i then Empty else
    if (getSizeJ lhs) < i then (dropJ i lhs) +++ rhs 
                          else dropJ i rhs
    
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i < 0 = Empty
takeJ i s@(Single b a) = if (getSize $ size b) == i then Empty else s 
takeJ i a@(Append b lhs rhs) = if (getSize $ size b) == i then a else
    if (getSizeJ lhs) < i then lhs ++ (takeJ i rhs)  
                          else takeJ i lhs

