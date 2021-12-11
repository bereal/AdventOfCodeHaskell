{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day11 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Attoparsec.Text (digit, endOfLine, many1, sepBy1)
import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import qualified Data.MultiSet as MS
import Data.Set ((\\))
import qualified Data.Set as S

type Coord = (Int, Int)

type Area = A.Array Coord Int

mkArea xs = let (h, w) = (length xs, length $ head xs) in A.listArray ((0, 0), (w - 1, h - 1)) $ concat xs

parser = ParsecParser $ mkArea <$> sepBy1 (many1 (digitToInt <$> digit)) endOfLine

neighbours :: Area -> Coord -> [Coord]
neighbours a (x, y) = filter (A.inRange $ A.bounds a) [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

-- find cells adjacent to any cell in a set,
-- count how many times each of them is a neighbour
countNeighbours :: Area -> S.Set Coord -> [(Coord, Int)]
countNeighbours a =
  MS.toOccurList
    . foldl (flip MS.insert) MS.empty
    . concatMap (neighbours a)
    . S.toList

-- flash all cells in a set,
-- find cells which are about to flash after that
flashSet :: Area -> S.Set Coord -> (Area, S.Set Coord)
flashSet a s =
  let n = countNeighbours a s
      a' = A.accum (+) a n
      newFlashes = S.fromList $ filter ((> 9) . (a' !)) $ map fst n
   in (a', newFlashes)

-- run the flashing phase, return all cells that flashed
flash :: Area -> (Area, S.Set Coord)
flash a =
  let sub flashed a (S.null -> True) = (a, flashed)
      sub flashed a q =
        let (a', q') = flashSet a q
            flashed' = S.union flashed q
         in sub flashed' a' (q' \\ flashed')
      firstToFlash = S.fromList $ map fst $ filter ((> 9) . snd) $ A.assocs a
   in sub S.empty a firstToFlash

step :: Area -> (Area, Int)
step a =
  let (afterFlash, flashed) = flash $ fmap (+ 1) a
      reset = afterFlash // map (,0) (S.toList flashed)
   in (reset, S.size flashed)

run a = tail $ iterate (step . fst) (a, 0)

solve1 = sum . map snd . take 100 . run

solve2 a = succ . fromJust . findIndex ((== length a) . snd) $ run a

solve = solveDay parser solve1 solve2
