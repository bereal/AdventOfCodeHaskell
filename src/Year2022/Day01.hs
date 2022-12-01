module Year2022.Day01 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (decimal, endOfLine, sepBy1)
import Data.List (sort)

parser = ParsecParser $ map sum <$> sepBy1 (sepBy1 decimal endOfLine) (endOfLine *> endOfLine)

solve1 :: [Int] -> Int
solve1 = maximum

solve2 = sum . take 3 . reverse . sort

solve = solveDay parser solve1 solve2
