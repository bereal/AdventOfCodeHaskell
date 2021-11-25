{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day04 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (Parser, char, decimal, digit)
import Data.Char (digitToInt)
import Data.List (group)

parser :: InputParser (Int, Int)
parser = ParsecParser $ (,) <$> decimal <* char '-' <*> decimal

intToList :: Int -> [Int]
intToList = map digitToInt . show

verify check = verify' False . group
  where
    verify' v (x : y : xs)
      | head x > head y = False
      | otherwise = verify' (v || check (length x)) (y : xs)
    verify' v [x] = v || check (length x)

solve' :: (Int -> Bool) -> (Int, Int) -> Int
solve' check (a, b) = length $ filter (verify check . intToList) [a .. b]

solve = solveDay parser (solve' (>= 2)) (solve' (== 2))
