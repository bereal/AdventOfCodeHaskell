module Year2021.Day15 where

import Common (InputParser (ParsecParser), solveDay)
import qualified Data.Array as A
import Data.Attoparsec.Text (digit, endOfLine, many1, sepBy1)
import Data.Bits (Bits ((.&.)), shift, (.|.))
import Data.Char (digitToInt)
import qualified Data.IntMap as IM
import qualified Data.IntPSQ as Q
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

type Cave = A.Array Coord Int

toCave xs =
  let (h, w) = (length xs, length $ head xs)
   in A.listArray ((0, 0), (h - 1, w - 1)) $ concat xs

multiplyCave :: Int -> [[Int]] -> [[Int]]
multiplyCave n xs =
  let (h, w) = (length xs, length $ head xs)
      incRow = map (succ . (`mod` 9))
      baseLine = map (concat . iterate incRow) xs
      nextLine = map (drop w)
      rows = concat $ take n $ iterate nextLine baseLine
   in map (take (w * n)) rows

adjacent arr (x, y) = filter (A.inRange $ A.bounds arr) [(x + 1, y), (x, y + 1), (x, y - 1), (x - 1, y)]

type Distances = IM.IntMap Int

type PQueue = Q.IntPSQ Int Int

coordKey (x, y) = (x `shift` 9) .|. y

keyToCoord x = (x `shift` (-9), x .&. 0x1ff)

updatePriority :: PQueue -> (Int, Int) -> PQueue
updatePriority pq (c, i) = snd $ Q.alter (const ((), Just (i, c))) c pq

notVisited :: PQueue -> Coord -> Bool
notVisited q c = Q.member (coordKey c) q

dijkstra :: Cave -> Int -> Int -> Distances -> PQueue -> Int
dijkstra cave current target dist queue =
  let currentDist = (IM.!) dist current
      neighbours = filter (notVisited queue) $ adjacent cave (keyToCoord current)
      localDist = IM.fromList $ [(coordKey n, currentDist + (A.!) cave n) | n <- neighbours]
      improvement = IM.union (IM.intersectionWith min localDist dist) localDist
      improvedDist = IM.union dist improvement
      updatedQ = foldl updatePriority queue $ IM.toList improvement
      Just (_, _, next, remaining) = Q.minView updatedQ
   in if next == target
        then (IM.!) improvedDist next
        else dijkstra cave next target improvedDist remaining

runDijkstra cave =
  let (_, (h, w)) = A.bounds cave
      start = (0, 0)
      target = (h, w)
      queue = Q.fromList $ [(coordKey (x, y), maxBound :: Int, coordKey (x, y)) | x <- [0 .. w], y <- [0 .. h]]
      dist = IM.singleton 0 0
      solved = dijkstra cave 0 (coordKey target) dist queue
   in solved

parser = ParsecParser $ sepBy1 (many1 $ digitToInt <$> digit) endOfLine

solve1 = runDijkstra . toCave

solve2 = runDijkstra . toCave . multiplyCave 5

solve = solveDay parser solve1 solve2
