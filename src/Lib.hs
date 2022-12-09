module Lib
    ( totalCalories
    , game
    ) where

import Data.List (groupBy, maximumBy, sortBy)

inputDay1 = "./input/day1.txt"
inputDay2 = "./input/day2.txt"

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

data Card a = Rock a | Paper a | Scissor a
 deriving (Show,Read)

toCard :: String -> Card Int
toCard a
  | a == "A" || a == "X" = Rock 1
  | a == "B" || a == "Y" = Paper 2
  | otherwise = Scissor 3

unCard (Rock x) = x
unCard (Paper x) = x
unCard (Scissor x) = x

game = do
  i <- readFile inputDay2
  putStrLn $ "day 2 part 1"
  print $ steps i
  print $ scoreInd $ steps i
  print $ score (steps i)
  where
    steps :: String -> [[Card Int]]
    steps cnt = map (\l -> map toCard $ words l) $ lines cnt
    score = sum . scoreInd
    scoreInd = map scoreSingle
    scoreSingle [a, b] = win a b
    win a b = case (a,b) of
      (Rock _, Paper y) -> 6 + y
      (Scissor _, Rock y) -> 6 + y
      (Paper _, Scissor y) -> 6 + y

      (Paper _, Rock y) -> 0 + y
      (Rock _, Scissor y) -> 0 + y
      (Scissor _, Paper y) -> 0 + y

      (Scissor _, Scissor y) -> 3 + y
      (Rock _, Rock y) -> 3 + y
      (Paper _, Paper y) -> 3 + y
