{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day02 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (endOfLine, sepBy1, inClass, satisfy, space)

parseMove = p "AX" 1 <|> p "BY" 2 <|> p "CZ" 3 where
    p s m = m <$ satisfy (inClass s)

parseRound = (,) <$> (parseMove <* space) <*> parseMove

parser :: InputParser [(Int, Int)]
parser = ParsecParser $ sepBy1 parseRound endOfLine

roundScore (1, 2) = 8
roundScore (2, 3) = 9
roundScore (3, 1) = 7
roundScore (a, b) | a == b = 3 + b
                  | otherwise  = b

convert2 (a, 2) = (a, a)
convert2 (a, 3) = (a, (a `mod` 3) + 1)
convert2 (a, 1) = (a, (a - 2) `mod` 3 + 1)

solve1 = sum . map roundScore

solve2 = solve1 . map convert2

solve = solveDay parser solve1 solve2
