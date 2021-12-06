{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day06 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)
import Data.List (group, sort)

parser = ParsecParser $ sepBy1 decimal $ char ','

next :: [Int] -> [Int]
next (zeros : rest) =
  let (prefix, [sevens, eights]) = splitAt 6 rest
   in prefix ++ [sevens + zeros] ++ [eights, zeros]

initCounter :: [Int] -> [Int]
initCounter = map (pred . length) . group . sort . (++ [0 .. 8])

run :: Int -> [Int] -> Int
run n = sum . (!! n) . iterate next . initCounter

solve = solveDay parser (run 80) (run 256)
