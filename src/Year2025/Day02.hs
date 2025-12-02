module Year2025.Day02 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)

range = (,) <$> decimal <* char '-' <*> decimal

parser :: InputParser [(Int, Int)]
parser = ParsecParser $ sepBy1 range $ char ','

isBad1 n =
  let s = show n
      (l, r) = length s `divMod` 2
   in ((r == 0) && uncurry (==) (splitAt l s))

getDivisors n = filter (\d -> n `mod` d == 0) [1 .. n - 1]

times n = concat . replicate n

isBad2 n =
  let s = show n
      len = length s
      check d = s == (len `div` d) `times` take d s
      ds = getDivisors len
   in any check ds

solve' isBad = let f (a, b) = filter isBad [a .. b] in sum . concatMap f

solve = solveDay parser (solve' isBad1) (solve' isBad2)