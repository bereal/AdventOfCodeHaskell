module Year2020.Day01 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (anyChar, decimal, endOfInput, endOfLine, many1, sepBy1)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)

parser = ParsecParser $ sepBy1 decimal endOfLine

findSum :: Int -> Int -> [Int] -> Maybe [Int]
findSum _ _ [] = Nothing
findSum target count (x : xs)
  | count == 0 = Nothing
  | x == target && count == 1 = Just [x]
  | x < target = case findSum (target - x) (count - 1) xs of
    Just found -> Just (x : found)
    _ -> findSum target count xs
  | otherwise = Nothing

solve' count xs = product $ fromJust $ findSum 2020 count (sort xs)

solve = solveDay parser (solve' 2) (solve' 3)
