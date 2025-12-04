module Year2025.Day04 where

import Common (InputParser (StringParser), solveDay)
import Data.List (elemIndices)
import Data.Set (Set, elems, fromList, member, (\\))

parser = StringParser $ pure . toSet . lines

toSet :: [String] -> Set (Int, Int)
toSet input = fromList [(row, col) | (row, r) <- zip [0 ..] input, col <- elemIndices '@' r]

getNeighbours (x, y) = [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

countNeighbours m = length . filter (`member` m) . getNeighbours

findRemovable input = filter ((< 4) . countNeighbours input) $ elems input

solve1 = length . findRemovable

solve2 = solve' 0
  where
    solve' n input = case findRemovable input of
      [] -> n
      removable -> solve' (n + length removable) $ input \\ fromList removable

solve = solveDay parser solve1 solve2