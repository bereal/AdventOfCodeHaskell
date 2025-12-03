module Year2024.Day02 where

import Common (InputParser, lineParser, skipPart, solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1, space)

parser :: InputParser [[Int]]
parser = lineParser $ sepBy1 decimal $ char ' '

-- isSafe :: [Int] -> Bool
isSafe vs =
  let grad = zipWith (-) vs $ tail vs
      (h : t) = grad
      monotonic = all ((== signum h) . signum) t
   in monotonic && all ((\v -> v >= 1 && v <= 3) . abs) grad

solve1 input = length $ filter isSafe input

solve = solveDay parser solve1 skipPart