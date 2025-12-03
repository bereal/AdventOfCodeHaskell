module Year2025.Day02 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)

range = (,) <$> decimal <* char '-' <*> decimal

parser :: InputParser [(Int, Int)]
parser = ParsecParser $ sepBy1 range $ char ','

times n = concat . replicate n

isBad :: (Int -> [Int]) -> Int -> Bool
isBad divisors n =
  let s = show n
      len = length s
      check d = s == (len `div` d) `times` take d s
      ds = divisors len
   in any check ds

solve' divisors =
  let f (a, b) = filter (isBad divisors) [a .. b]
   in sum . concatMap f

solve1 = solve' $ \n -> let (d, r) = n `divMod` 2 in [d | r == 0]

solve2 = solve' $ \n -> filter ((== 0) . mod n) [1 .. n - 1]

solve = solveDay parser solve1 solve2