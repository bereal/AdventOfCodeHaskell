module Year2021.Day01 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (decimal, endOfLine, sepBy1)

parser = ParsecParser $ sepBy1 decimal endOfLine

-- How it works:
-- https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads
countIncreases :: Int -> [Int] -> Int
countIncreases step = length . filter id . (zipWith (<) <*> drop step)

solve = solveDay parser (countIncreases 1) (countIncreases 3)
