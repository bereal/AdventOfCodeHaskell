module Year2025.Day05 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1)
import Data.List (sort)

range = (,) <$> decimal <* char '-' <*> decimal

parser :: InputParser ([(Int, Int)], [Int])
parser = ParsecParser $ (,) <$> (sepBy1 range endOfLine <* endOfLine <* endOfLine) <*> sepBy1 decimal endOfLine

inRange n (a, b) = n >= a && n <= b

fresh ranges n = any (inRange n) ranges

solve1 (ranges, numbers) = length $ filter (fresh ranges) numbers

merge :: [(Int, Int)] -> [(Int, Int)]
merge = foldl merge' []
  where
    merge' [] a = [a]
    merge' acc@((a, b) : t) h@(c, d)
      | c <= b = (a, max b d) : t
      | otherwise = h : acc

rangeSize (a, b) = b - a + 1

solve2 = sum . map rangeSize . merge . sort . fst

solve = solveDay parser solve1 solve2