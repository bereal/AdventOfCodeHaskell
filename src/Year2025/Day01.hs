module Year2025.Day01 where

import Common (InputParser (HardCoded, StringParser), lineParser, solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, decimal, string)
import Data.Functor (($>))
import Debug.Trace (trace)

left = negate <$> (char 'L' *> decimal)

right = char 'R' *> decimal

parser :: InputParser [Int]
parser = lineParser (left <|> right)

step (counter, pos) shift
  | shift > 0 =
      let pos' = pos + shift
          c = pos' `div` 100 - pos `div` 100
       in (counter + c, pos' `mod` 100)
  | otherwise =
      let (c, pos') = step (counter, -pos) (-shift)
       in (c, -pos')

solve1 = length . filter (== 0) . map snd . scanl step (0, 50)

solve2 = fst . foldl step (0, 50)

solve = solveDay parser solve1 solve2