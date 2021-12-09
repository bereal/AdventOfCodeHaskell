module Year2021.Day09 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (digit, endOfInput, endOfLine, many1, sepBy1)
import Data.Char (digitToInt)
import Data.List (sort, sortOn)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Set ((\\))
import qualified Data.Set as S

parser = ParsecParser $ toMap <$> sepBy1 (many1 (digitToInt <$> digit)) endOfLine

type Point = (Int, Int)

type Area = M.Map Point Int

toMap :: [[Int]] -> Area
toMap as = M.fromList [((row, col), n) | (r, row) <- zip as [0 ..], (n, col) <- zip r [0 ..]]

adjacent :: Area -> Point -> [Point]
adjacent m (x, y) = filter (`M.member` m) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLocalMinimum :: Area -> Point -> Bool
isLocalMinimum m p = (m ! p) < minimum (map (m !) $ adjacent m p)

findPointBasin :: Area -> Point -> S.Set Point
findPointBasin m p = find' (S.singleton p) p
  where
    find' s p =
      let adj = S.filter ((/= 9) . (m !)) $ S.fromList (adjacent m p)
          sub seen n
            | n `S.member` seen = seen
            | otherwise = find' (S.insert n seen) n
       in foldl sub s adj

findBasins :: Area -> [S.Set Point]
findBasins m =
  let allPoints = S.fromList [p | (p, v) <- M.toList m, v /= 9]
      find basins points
        | S.null points = basins
        | otherwise =
          let p = S.elemAt 0 points
              b = findPointBasin m p
           in find (b : basins) (points \\ b)
   in find [] allPoints

solve1 m =
  let points = filter (isLocalMinimum m) (M.keys m)
   in sum $ map ((+ 1) . (m !)) points

solve2 m =
  let basins = findBasins m
      top3 = take 3 $ reverse $ sort $ map S.size basins
   in product top3

solve = solveDay parser solve1 solve2
