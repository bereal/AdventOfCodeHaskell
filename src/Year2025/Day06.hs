module Year2025.Day06 where

import Common (InputParser (ParsecParser), skipPart, solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, many1, sepBy1, skipMany)

type Operation = Int -> Int -> Int

whitespace = many1 $ char ' '

mul :: Parser Operation
mul = (*) <$ char '*'

add :: Parser Operation
add = (+) <$ char '+'

opLine = sepBy1 (mul <|> add) whitespace

numLine :: Parser [Int]
numLine = sepBy1 decimal whitespace <* skipMany (char ' ')

parser :: InputParser ([[Int]], [Operation])
parser = ParsecParser $ (,) <$> sepBy1 numLine endOfLine <* endOfLine <*> opLine

pivot = pivot' []
  where
    pivot' acc ([] : _) = reverse acc
    pivot' acc xs = pivot' (map head xs : acc) (map tail xs)

solve1 (nums, ops) = sum $ zipWith (flip foldl1) (pivot nums) ops

solve = solveDay parser solve1 skipPart
