--module fifth where

import Data.List
import Data.Char
import System.IO

data Person = Person { first :: String, second :: String }
    deriving (Show)

main :: IO () 
main = do 
    --let linzhe = Person { first = "Linzhe", second = "Cai" }
    inh <- openFile "Parser.hs" ReadMode 
    outh <- openFile "Calc.hs" WriteMode 
    mainLoop inh outh
    hClose inh
    hClose outh
 
    interact (map toUpper)

mainLoop :: Handle -> Handle -> IO ()
mainLoop inh outh = do
    inEof <- hIsEOF inh
    if inEof 
    then return ()
    else do inStr <- hGetLine inh
            hPutStrLn outh (map toUpper inStr)
            mainLoop inh outh
        
    --let filename = "Parser.hs"
    --file <- readFile filename
    --putStr $ unlines file
foo :: Int -> Int -> Int
foo n = \x -> n + x
