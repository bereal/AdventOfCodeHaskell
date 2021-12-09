{-# LANGUAGE ViewPatterns #-}

-- See also branch 2021-09-grid for a solution with Math.Geometry.Grid

module Year2021.Day09 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (digit, endOfInput, endOfLine, many1, sepBy1)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Set ((\\))
import qualified Data.Set as S

parser = ParsecParser $ sepBy1 (many1 (digitToInt <$> digit)) endOfLine

type Point = (Int, Int)

toMap :: [[Int]] -> M.Map Point Int
toMap as = M.fromList [((row, col), n) | (r, row) <- zip as [0 ..], (n, col) <- zip r [0 ..], n /= 9]

toSet :: [[Int]] -> S.Set Point
toSet = S.fromList . M.keys . toMap

adjacent :: Point -> [Point]
adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

adjacentInSet s = filter (`S.member` s) . adjacent

adjacentInMap m = filter (`M.member` m) . adjacent

isLocalMinimum :: M.Map Point Int -> Point -> Bool
isLocalMinimum m p = let v = (m ! p) in all ((v <) . (m !)) $ adjacentInMap m p

findBasin :: S.Set Point -> S.Set Point
findBasin (S.null -> True) = S.empty
findBasin m =
  let sub seen ((`S.member` seen) -> True) = seen
      sub seen n = find (S.insert n seen) n
      find seen p = foldl sub seen $ adjacentInSet m p
      start = S.elemAt 0 m
   in find (S.singleton start) start

findAllBasins :: S.Set Point -> [S.Set Point]
findAllBasins m =
  let find s = let b = findBasin s in (b : find (s \\ b))
   in takeWhile (not . S.null) $ find m

solve1 inp =
  let m = toMap inp
      points = filter (isLocalMinimum m) (M.keys m)
   in sum $ map ((+ 1) . (m !)) points

solve2 = product . take 3 . reverse . sort . map S.size . findAllBasins . toSet

solve = solveDay parser solve1 solve2
