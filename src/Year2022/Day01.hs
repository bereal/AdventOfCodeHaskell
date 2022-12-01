module Year2022.Day01 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (decimal, endOfLine, sepBy1)
import Data.List (sort)

parser :: InputParser [Int]
parser = ParsecParser $ reverse . sort . map sum <$> sepBy1 (sepBy1 decimal endOfLine) (endOfLine *> endOfLine)

solve = solveDay parser head (sum . take 3)
