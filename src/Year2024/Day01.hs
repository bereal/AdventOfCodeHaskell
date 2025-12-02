module Year2024.Day01 where

import Common (InputParser, lineParser, solveDay)
import Data.Attoparsec.Text (decimal, many1, space)
import Data.List (sort)
import qualified Data.MultiSet as MultiSet

pair = (,) <$> (decimal <* many1 space) <*> decimal

parser :: InputParser [(Int, Int)]
parser = lineParser pair

solve1 input =
  let left = sort $ map fst input
      right = sort $ map snd input
   in sum $ map abs $ zipWith (-) left right

solve2 input =
  let left = map fst input
      right = MultiSet.fromList $ map snd input
   in sum $ map (\a -> a * MultiSet.occur a right) left

solve = solveDay parser solve1 solve2