module Year2015.Day01 where

import Common (InputParser (ParsecParser), lineParser, solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, many1)
import Data.Functor (($>))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

bracket = char '(' $> 1 <|> char ')' $> (-1)

parser :: InputParser [Int]
parser = ParsecParser $ many1 bracket

solve1 = sum

solve2 = fromJust . elemIndex (-1) . scanl (+) 0

solve = solveDay parser solve1 solve2