module Lib
    ( totalCalories
    ) where

import Data.List (groupBy, maximumBy)

inputDay1 = "./input/day1.txt"

totalCalories :: IO ()
totalCalories = do
  i <- readFile inputDay1
  putStrLn $ "hello"
  print $ elvesCal $ elvesBag i
  print $ maxCaloriesElv $ elvesCal $ elvesBag i
  return ()
  where
    elvesBag cnt = zip [1..] $ map cnv $ groupBy (\_ b -> b /= "") (lines cnt)
    cnv :: [String] -> [Int]
    cnv lst = map (\x -> read x :: Int) $ filter (/="") lst
    elvesCal :: [(Int, [Int])] -> [(Int, Int)]
    elvesCal xs = map (\(a, lst) -> (a, sum lst)) xs

maxCaloriesElv :: [(Int, Int)] -> (Int, Int)
maxCaloriesElv xs = maximumBy (\(a,b) (c,d) -> compare b d) xs
