module Utils where

splitInts :: Char -> String -> [Int]
splitInts d [] = []
splitInts d s = read x : splitInts d (drop 1 y) where (x,y) = span (/= d) s
