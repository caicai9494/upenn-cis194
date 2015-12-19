{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

--Ex1

parseMessage :: String -> LogMessage
parseMessage str = case slist of
    ("I": i : rest) -> LogMessage Info (read i) (unwords rest)
    ("W": i : rest) -> LogMessage Warning (read i) (unwords rest)
    ("E": e: i: rest) -> LogMessage (Error (read e)) (read i) (unwords rest)
    _ -> Unknown str
    where slist :: [String]
          slist = words str
          
parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

--Ex2
messageCompare :: LogMessage -> LogMessage -> Bool
messageCompare (LogMessage _ ltime _) (LogMessage _ rtime _) = ltime < rtime
messageCompare _ _ = False -- this shall never happen

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree 
insert msg tree = traverse tree
    where traverse :: MessageTree -> MessageTree
          traverse Leaf = Node Leaf msg Leaf 
          traverse (Node ltree msg' rtree) = if messageCompare msg msg'  
					     then Node (traverse ltree) msg' rtree
					     else Node ltree msg' (traverse rtree)
        

--Ex3
build :: [LogMessage] -> MessageTree
build = foldl (\acc x -> insert x acc) Leaf  

--Ex4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = (inOrder lt) ++ [msg] ++ (inOrder rt)

--Ex5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = foldr errMoreThan50 [] (inOrder $ build msgs)
    where errMoreThan50 :: LogMessage -> [String] -> [String]
          errMoreThan50 (LogMessage (Error i) _ str) strs = if i > 50 then (str:strs) else strs
          errMoreThan50 _ strs = strs
               

main :: IO [String]
main = do 
       contents <- readFile "error.log" 
       let msgs = parse contents
       --let mtree = build msgs
       --return (inOrder mtree)
       return (whatWentWrong msgs)
       
