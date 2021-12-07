module Year2021.Day07 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, endOfInput, sepBy1)

parser = ParsecParser $ sepBy1 decimal (char ',')

evalFuel :: (Int -> Int -> Int) -> [Int] -> Int -> Int
evalFuel cost coords target = sum $ map (`cost` target) coords

cost1 a b = abs (a - b)

cost2 a b = let n = abs (a - b) in (n + 1) * n `div` 2

findMin cost coords =
  let a = minimum coords
      b = maximum coords
   in minimum $ map (evalFuel cost coords) [a .. b]

solve = solveDay parser (findMin cost1) (findMin cost2)
