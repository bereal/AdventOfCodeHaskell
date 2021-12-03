module Year2021.Day03 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, endOfInput, endOfLine, many1, sepBy1)
import Data.Bits (shift)
import Data.Functor (($>))
import Data.List (partition)
import Data.Text (Text)

parser = ParsecParser $ sepBy1 (many1 $ char '0' $> 0 <|> char '1' $> 1) endOfLine

bitsToInt :: [Int] -> Int
bitsToInt = foldl1 ((+) . (`shift` 1))

solve1 x = rate (> 0) * rate (< 0)
  where
    zeroToMinus1 a = a - fromEnum (a == 0)
    summary = foldl1 (zipWith ((. zeroToMinus1) . (+))) x
    rate f = bitsToInt $ map (fromEnum . f) summary

filterNums _ [x] = x
filterNums choose nums =
  let chosen = choose nums
      bit = head $ head chosen
      subs = map tail chosen
   in bit : filterNums choose subs

orderBy1stBitPrevalence num =
  let p@(zs, os) = partition ((== 0) . head) num
   in if length zs > length os then (os, zs) else p

minority = fst . orderBy1stBitPrevalence

majority = snd . orderBy1stBitPrevalence

solve2 xs =
  let find f = bitsToInt $ filterNums f xs
   in find majority * find minority

solve = solveDay parser solve1 solve2
