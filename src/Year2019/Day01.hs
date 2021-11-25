module Year2019.Day01 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (anyChar, decimal, endOfInput, endOfLine, many1, sepBy1)
import Data.Text (Text)

parser = ParsecParser $ sepBy1 decimal endOfLine

fuel :: Int -> Int
fuel n = truncate (fromIntegral n / 3) - 2

fuelRec :: Int -> Int
fuelRec n =
  let f = fuel n
   in if f <= 0 then 0 else f + fuelRec f

solve1 = sum . map fuel

solve2 = sum . map fuelRec

solve = solveDay parser solve1 solve2
