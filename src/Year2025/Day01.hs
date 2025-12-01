module Year2025.Day01 where

import Common (InputParser, lineParser, solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, decimal)

left = negate <$> (char 'L' *> decimal)

right = char 'R' *> decimal

parser :: InputParser [Int]
parser = lineParser (left <|> right)

countZeros a b =
  let (l, r) = if a < b then (a, b) else (-a, -b)
   in r `div` 100 - l `div` 100

solve1 = length . filter (== 0) . map (`mod` 100) . scanl (+) 50

solve2 input =
  let path = scanl (+) 50 input
   in sum $ zipWith countZeros path $ tail path

solve = solveDay parser solve1 solve2