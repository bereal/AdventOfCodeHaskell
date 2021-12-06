{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day06 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)
import Data.List (group, sort)

parser = ParsecParser $ sepBy1 decimal $ char ','

next [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, h + a, i, a]

initCounter = map (pred . length) . group . sort . (++ [0 .. 8])

run :: Int -> [Int] -> Int
run n = sum . (!! n) . iterate next . initCounter

solve = solveDay parser (run 80) (run 256)
