module Year2025.Day03 where

import Common (InputParser (StringParser), solveDay)
import Data.Foldable (maximumBy)
import Data.List (tails)
import Data.Ord (comparing)

parser :: InputParser [[Int]]
parser = StringParser $ pure . map (map (read . (: []))) . lines

-- find the sublist starting with the max digit
findBestTail = maximumBy (comparing head) . tail . reverse . tails

digitsToNum ds = sum $ zipWith (*) ds $ map (10 ^) [0 ..]

findBest = find' []
  where
    find' acc 0 _ = digitsToNum acc
    find' acc len s =
      let (left, right) = splitAt (length s - len + 1) s
          (d : t) = findBestTail left
       in find' (d : acc) (len - 1) (t ++ right)

solve' len = sum . map (findBest len)

solve = solveDay parser (solve' 2) (solve' 12)