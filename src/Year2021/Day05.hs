{-# LANGUAGE TupleSections #-}

module Year2021.Day05 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1, string)
import Data.List (group, sort)

type Point = (Int, Int)

type Line = (Point, Point)

point = (,) <$> decimal <* char ',' <*> decimal

line = (,) <$> point <* string " -> " <*> point

parser :: InputParser [Line]
parser = ParsecParser $ sepBy1 line endOfLine

range a b = [a, a + signum (b - a) .. b]

draw1 v@((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = draw2 v
  | otherwise = []

draw2 ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)

countRepeats :: [Point] -> Int
countRepeats = length . filter ((> 1) . length) . group . sort

solve1 = countRepeats . concatMap draw1

solve2 = countRepeats . concatMap draw2

solve = solveDay parser solve1 solve2
