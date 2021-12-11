{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day11 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Arrow ((&&&))
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Attoparsec.Text (digit, endOfLine, many1, sepBy1)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (elemIndex, group, sort)
import Data.Maybe (fromJust)
import Data.Set ((\\))
import qualified Data.Set as S

type Coord = (Int, Int)

type Area = A.Array Coord Int

mkArea xs =
  let (h, w) = (length xs, length $ head xs)
   in A.listArray ((0, 0), (w - 1, h - 1)) $ concat xs

parser = ParsecParser $ mkArea <$> sepBy1 (many1 (digitToInt <$> digit)) endOfLine

neighbours :: Area -> Coord -> [Coord]
neighbours a (x, y) = filter (A.inRange $ A.bounds a) [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

counts :: (Ord a, Foldable t) => t a -> [(a, Int)]
counts = map (head &&& length) . group . sort . toList

-- find cells adjacent to any cell in a set,
-- count how many times each of them is a neighbour
countNeighbours :: Area -> S.Set Coord -> [(Coord, Int)]
countNeighbours a = counts . concatMap (neighbours a)

-- flash all cells in a set,
-- find cells which are about to flash after that
flashSet :: Area -> S.Set Coord -> (Area, S.Set Coord)
flashSet a s =
  let n = countNeighbours a s
      a' = A.accum (+) a n
      newFlashes = S.fromList $ filter ((> 9) . (a' !)) $ map fst n
   in (a', newFlashes)

-- run the flashing phase, return all cells that flashed
flash :: Area -> (Area, [Coord])
flash area =
  let recur flashed a (S.null -> True) = (a, toList flashed)
      recur flashed a q =
        let (a', q') = flashSet a q
            flashed' = S.union flashed q
         in recur flashed' a' (q' \\ flashed')
      firstToFlash = S.fromList $ map fst $ filter ((> 9) . snd) $ A.assocs area
   in recur S.empty area firstToFlash

reset area flashed = (area // map (,0) flashed, flashed)

run a = tail $ map snd $ iterate (fmap length . uncurry reset . flash . fmap (+ 1) . fst) (a, 0)

solve1 = sum . take 100 . run

solve2 a = succ . fromJust . elemIndex (length a) $ run a

solve = solveDay parser solve1 solve2
