module Year2021.Day03 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, endOfInput, endOfLine, many1, sepBy1)
import Data.Bits (shift)
import Data.Functor (($>))
import Data.List (partition)
import Data.Text (Text)

parser = ParsecParser $ sepBy1 (many1 $ char '0' $> 0 <|> char '1' $> 1) endOfLine

type Bits = [Int]

bitsToInt :: Bits -> Int
bitsToInt = foldl1 ((+) . (`shift` 1))

solve1 :: [Bits] -> Int
solve1 x = rate (> 0) * rate (< 0)
  where
    zeroToMinus1 a = a - fromEnum (a == 0)
    summary = foldl1 (zipWith ((. zeroToMinus1) . (+))) x
    rate f = bitsToInt $ map (fromEnum . f) summary

sieve :: (Int -> Int -> Bool) -> [Bits] -> Bits
sieve _ [x] = x
sieve choose nums =
  let (zeros, ones) = partition ((== 0) . head) nums
      chosen = if length zeros `choose` length ones then zeros else ones
      bit = head $ head chosen
      subs = map tail chosen
   in bit : sieve choose subs

solve2 xs =
  let find f = bitsToInt $ sieve f xs
   in find (<) * find (>)

solve = solveDay parser solve1 solve2
