{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Play where

import Data.List
import Data.Text       qualified as Text
import Data.Map.Strict qualified as Map

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- splitR :: String -> String -> [String]
-- splitR exp = go
--        where

--         n :: Int
--         n = length exp

--         go :: String -> [String]
--         go "" = []
--         go rContent
--            | take n rContent == exp = "" : go (drop n rContent)
--            | otherwise              = let result = go (tail rContent) in


        -- go :: String -> [String]
        -- go []                       = []
        -- go rContent
        --    | take n rContent /= exp = case go (tail rContent) of
        --                                 [] -> (head rContent : "") : []
        --                                 (x:xs) -> (head rContent : x) : xs
        --    | otherwise              = "" : go (drop n rContent)

{-

"  "
"This  is  a  text."

-}
splitR :: String -> String -> [String]
splitR exp = undefined
          
-- splitR :: String -> String -> [String]
-- splitR exp = undefined
--        where

--        f :: Char -> (String,[String]) -> (String,[String])
--        f c (str,vs)
--          | 
       
        
       -- where
       --  expLength :: Int
       --  expLength = length exp

       --  f :: Char -> [String] -> [String]
       --  f c []         = [[c]]
       --  f c (str:strs)
       --    | str == exp = [c]:strs
       --    | otherwise  = (c:str):strs
        

morseCodes :: Map.Map String String
morseCodes = undefined

decodeMorse :: String -> String
decodeMorse = unwords
            . fmap (concatMap (morseCodes Map.!) . words . Text.unpack)
            . Text.splitOn "   "
            . Text.strip
            . Text.pack

-- decodeMorse :: String -> String
-- decodeMorse = unwords
--             . fmap (mconcat . map (morseCodes Map.!) . words)
--             . splitR "   "
--             . dropWhileEnd (== ' ')
--             . dropWhile (== ' ')





type LastDigits = Map.Map Integer (Integer -> Integer)
digitMap :: LastDigits
digitMap = mkDigitMap <$> Map.fromList
           [ (1, [1])
           , (2, [2,4,8,6])
           , (3, [3,9,7,1])
           , (4, [4,6])
           , (5, [5])
           , (6, [6])
           , (7, [7,9,3,1])
           , (8, [8,4,2,6])
           , (9, [9,1])
           , (0, [0])
           ]

lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit a b = digitMap Map.! (read [last (show a)]) $ b

{-
2 4 8 16
1 2 3 4

3 9 27 81
1 2 3  4

4 16 64 256
1 2  3  4

5 = 5

6 = 6

7 49 343 2401

8 64 512 4096

9 81 729 6561

-}


-- combinations :: Int -> [Int] -> [[Int]]
-- combinations 0 _  = []
-- combinations 1 xs = map (:[]) xs
-- combinations n 

