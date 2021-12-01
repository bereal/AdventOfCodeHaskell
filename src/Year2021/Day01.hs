module Year2021.Day01 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (decimal, endOfLine, sepBy1)

parser = ParsecParser $ sepBy1 decimal endOfLine

solve1 xs = length $ filter (< 0) $ zipWith (-) xs $ tail xs

solve2 = solve1 . slidingWindow
  where
    slidingWindow (x : xs@(y : z : _)) = (x + y + z) : slidingWindow xs
    slidingWindow _ = []

solve = solveDay parser solve1 solve2
