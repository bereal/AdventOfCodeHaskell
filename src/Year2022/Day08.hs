{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day08 where
import Common (lineParser, solveDay, skipPart)
import Data.Attoparsec.Text (Parser, digit, many1)
import qualified Data.Map as M
import Data.List (sort, transpose)
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Tuple (swap)
import qualified Data.Set as S

parser = lineParser $ many1 $ digitToInt <$> digit

findLeftVisibleIndices :: [Int] -> [Int]
findLeftVisibleIndices row =
    map fst $
    filter snd $
    zip [1..] $
    zipWith (>) (tail $ init row) $
    scanl1 max $ init row

findVisibleIndices :: [Int] -> [Int]
findVisibleIndices row = let len = length row
    in findLeftVisibleIndices row ++ map (len-1-) (findLeftVisibleIndices $ reverse row)

findVisibleIndicesInRows :: [[Int]] -> [[Int]]
findVisibleIndicesInRows = map findVisibleIndices . tail . init

findVisible :: [[Int]] -> [(Int, Int)]
findVisible grid = f grid ++ map swap (f $ transpose grid)
    where f = concat . zipWith (map . (,)) [1..] . findVisibleIndicesInRows

solve1 grid = let
        w = length $ head grid
        h = length grid
    in (h + w) * 2 - 4 + S.size (S.fromList $ findVisible grid)

solve = solveDay parser solve1 skipPart