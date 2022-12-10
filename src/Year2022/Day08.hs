module Year2022.Day08 where
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (Parser, digit, many1)
import Data.List (transpose)
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

countLeftVisibility :: [Int] -> [Int]
countLeftVisibility row = reverse $ snd $ foldl cv ([9], []) $ tail $ init row where
    cv (hist, buf) i = let v = length $ takeWhile (< i) hist in (i:hist, (v+1):buf)

countVisibilityInRow :: [Int] -> [(Int, Int)]
countVisibilityInRow row = zip (countLeftVisibility row) (reverse $ countLeftVisibility $ reverse row)

countVisibility :: [[Int]] -> [[(Int, Int)]]
countVisibility = map countVisibilityInRow . tail . init

solve1 grid = let
        w = length $ head grid
        h = length grid
    in (h + w) * 2 - 4 + S.size (S.fromList $ findVisible grid)

solve2 grid = let
    rv = concat $ countVisibility grid
    cv = concat $ transpose $ countVisibility $ transpose grid
 in maximum $ zipWith (\(a, b) (c, d) -> a * b * c * d) rv cv

solve = solveDay parser solve1 solve2