sumTo20 :: [Int] -> Int
sumTo20 xs = go xs 0 
    where go :: [Int] -> Int -> Int 
	  go [] acc = acc
	  go (x:xs) acc 
	   | acc >= 20 = acc 
	   | otherwise = go xs (acc+x)
	      
length2 :: [a] -> Int
length2 xs = foldr (\_ y -> y + 1) 0 xs

f :: Int -> Int
f n = (*2) n
