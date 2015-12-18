{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

xorBs :: ByteString -> ByteString -> ByteString
xorBs lhs rhs = BS.pack $ BS.zipWith xor lhs rhs

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret oldp newp = do 
    oldstr <- BS.readFile oldp 
    newstr <- BS.readFile newp
    return $ BS.filter (/=0) $ xorBs oldstr newstr 

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key ofile = do 
    crypted <- BS.readFile "victims.json.enc"
    BS.writeFile ofile $ xorBs crypted $ BS.cycle key
     

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jpath = do
    json <- BS.readFile jpath 
    return $ decode json 


-- Exercise 4 -----------------------------------------
isBadT :: Transaction -> [TId] -> Bool
isBadT (Transaction {amount=_, to=_, from=_, tid=t}) tids = t `elem` tids  

getBadTrans :: Maybe [Transaction] -> Maybe [TId] -> Maybe [Transaction]
getBadTrans Nothing _ = Nothing
getBadTrans _ Nothing = Nothing
getBadTrans (Just ts) (Just is) = Just (helper ts is) where
    helper :: [Transaction] -> [TId] -> [Transaction]
    helper ts' is' = filter (\t -> isBadT t is')  ts' 
  



getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vpath tpath = do
    vjson <- parseFile vpath :: IO (Maybe [TId])
    tjson <- parseFile tpath :: IO (Maybe [Transaction])
    return $ getBadTrans tjson vjson

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty 
getFlow ts = Map.fromList $ map (\(Transaction {amount=a, to=_, from=f, tid=t}) -> (f, a)) ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.findMax m

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
	  --putStrLn $ getCriminal flow
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

