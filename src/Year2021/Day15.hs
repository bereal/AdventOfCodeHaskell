module Year2021.Day15 where

import Common (InputParser (ParsecParser), solveDay)
import qualified Data.Array as A
import Data.Attoparsec.Text (digit, endOfLine, many1, sepBy1)
import Data.Bits (shift, (.|.))
import Data.Char (digitToInt)
import qualified Data.IntPSQ as Q
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

type Cave = A.Array Coord Int

toCave xs =
  let (h, w) = (length xs, length $ head xs)
   in A.listArray ((0, 0), (h - 1, w - 1)) $ concat xs

multiplyCave :: [[Int]] -> [[Int]]
multiplyCave xs =
  let (h, w) = (length xs, length $ head xs)
      incRow = map (succ . (`mod` 9))
      baseLine = map (concat . iterate incRow) xs
      nextLine = map (drop w)
      rows = concat $ take 5 $ iterate nextLine baseLine
   in map (take (w * 5)) rows

adjacent arr (x, y) = filter (A.inRange $ A.bounds arr) [(x + 1, y), (x, y + 1), (x, y - 1), (x - 1, y)]

type Distances = M.Map Coord Int

type PQueue = Q.IntPSQ Int Coord

coordKey (x, y) = (x `shift` 9) .|. y

updatePriority :: PQueue -> (Coord, Int) -> PQueue
updatePriority pq (c, i) = snd $ Q.alter (const ((), Just (i, c))) (coordKey c) pq

notVisited :: PQueue -> Coord -> Bool
notVisited q c = Q.member (coordKey c) q

dijkstra :: Cave -> Coord -> Coord -> Distances -> PQueue -> Int
dijkstra cave current target dist queue =
  let currentDist = (M.!) dist current
      neighbours = filter (notVisited queue) $ adjacent cave current
      localDist = M.fromList [(n, currentDist + (A.!) cave n) | n <- neighbours]
      improvement = M.union (M.intersectionWith min localDist dist) localDist
      improvedDist = M.union dist improvement
      updatedQ = foldl updatePriority queue $ M.toList improvement
      Just (_, _, next, remaining) = Q.minView updatedQ
   in if next == target
        then (M.!) improvedDist target
        else dijkstra cave next target improvedDist remaining

runDijkstra cave =
  let (_, (h, w)) = A.bounds cave
      start = (0, 0)
      target = (h, w)
      queue = Q.fromList $ [(coordKey (x, y), maxBound :: Int, (x, y)) | x <- [0 .. w], y <- [0 .. h]]
      dist = M.singleton start 0
      solved = dijkstra cave start target dist queue
   in solved

parser = ParsecParser $ sepBy1 (many1 $ digitToInt <$> digit) endOfLine

solve1 = runDijkstra . toCave

solve2 = runDijkstra . toCave . multiplyCave

solve = solveDay parser solve1 solve2
