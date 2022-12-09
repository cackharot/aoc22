module Lib
    ( totalCalories
    ) where

import Data.List (groupBy, maximumBy, sortBy)

inputDay1 = "./input/day1.txt"

totalCalories :: IO ()
totalCalories = do
  i <- readFile inputDay1
  putStrLn $ "day 1 part 1"
  -- print $ elvesCal $ elvesBag i
  print $ maxCaloriesElv $ day1Part1 i
  putStrLn $ "day 1 part 2"
  print $ top3 $ day1Part1 i
  where
    day1Part1 = elvesCal . elvesBag
    elvesBag cnt = zip [1..] $ map cnv $ groupBy (\_ b -> b /= "") (lines cnt)
    cnv :: [String] -> [Int]
    cnv lst = map (\x -> read x :: Int) $ filter (/="") lst
    elvesCal :: [(Int, [Int])] -> [(Int, Int)]
    elvesCal xs = map (\(a, lst) -> (a, sum lst)) xs

maxCaloriesElv :: [(Int, Int)] -> (Int, Int)
maxCaloriesElv xs = maximumBy (\(a,b) (c,d) -> compare b d) xs

-- top3 :: [(Int, Int)] -> Int
top3 xs = let t = take 3 $ sortBy (\(a,b) (c,d) -> compare d b) xs in
  foldr (\(a,b) i -> i+b) 0 t
