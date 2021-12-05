{-# LANGUAGE TupleSections #-}

module Year2021.Day05 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1, string)
import qualified Data.Map as M
import Data.Text (Text)

type Point = (Int, Int)

type Line = (Point, Point)

type Map = M.Map Point Int

point = (,) <$> decimal <* char ',' <*> decimal

line = (,) <$> point <* string " -> " <*> point

parser :: InputParser [Line]
parser = ParsecParser $ sepBy1 line endOfLine

range a b = [a, a + signum (b - a) .. b]

drawLine :: Bool -> Line -> [Point]
drawLine useDiag ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) $ range y1 y2
  | y1 == y2 = map (,y1) $ range x1 x2
  | otherwise = if useDiag then zip (range x1 x2) (range y1 y2) else []

updateMap :: Map -> [Point] -> Map
updateMap = foldl (\m p -> M.insertWith (+) p 1 m)

countIntersections :: Bool -> [Line] -> Int
countIntersections useDiag = length . M.filter (> 1) . updateMap M.empty . concatMap (drawLine useDiag)

solve = solveDay parser (countIntersections False) (countIntersections True)
