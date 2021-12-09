module Year2021.Day09 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Array (Array, indices, listArray, (!))
import Data.Attoparsec.Text (digit, endOfInput, endOfLine, many1, sepBy1)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set ((\\))
import qualified Data.Set as S
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

parser = ParsecParser $ mkArea <$> sepBy1 (many1 (digitToInt <$> digit)) endOfLine

type Point = (Int, Int)

data Area = Area {points :: Array Point Int, grid :: RectSquareGrid} deriving (Show)

(!~) :: Area -> Point -> Int
(!~) a p = points a ! p

mkArea :: [[Int]] -> Area
mkArea vs =
  let bound@(h, w) = (length vs, length $ head vs)
   in Area (listArray ((0, 0), (h -1, w -1)) $ concat vs) (rectSquareGrid w h)

adjacent :: Area -> Point -> [Point]
adjacent = neighbours . grid

findLocalMinima area = filter (isLocalMinimum area) (indices $ points area)

isLocalMinimum :: Area -> Point -> Bool
isLocalMinimum m p = let v = (m !~ p) in all ((v <) . (m !~)) (adjacent m p)

findBasin :: Area -> Point -> S.Set Point
findBasin a start =
  let sub seen p
        | (a !~ p) /= 9 && not (p `S.member` seen) = find (S.insert p seen) p
        | otherwise = seen
      find seen p = foldl sub seen $ adjacent a p
   in find (S.singleton start) start

findAllBasins :: Area -> [S.Set Point]
findAllBasins a = map (findBasin a) $ findLocalMinima a

solve1 area =
  let ps = filter (isLocalMinimum area) (indices $ points area)
   in sum $ map ((+ 1) . (area !~)) ps

solve2 = product . take 3 . reverse . sort . map S.size . findAllBasins

solve = solveDay parser solve1 solve2
