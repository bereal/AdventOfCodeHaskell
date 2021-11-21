module Year2021.Day15 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (anyChar, endOfInput, many1)
import Data.Text (Text)

parser = ParsecParser $ many1 anyChar

solve = skipDay
