module Year2015.Day02 where

import Common (InputParser (ParsecParser), lineParser, solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, decimal, many1)
import Data.Functor (($>))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

dimensions = (,,) <$> decimal <* char 'x' <*> decimal <* char 'x' <*> decimal

parser :: InputParser [(Int, Int, Int)]
parser = lineParser dimensions

wrap (w, l, h) =
  let sides = [w * l, l * h, h * w]
   in minimum sides + 2 * sum sides

ribbon (w, l, h) =
  let sides = [w + l, l + h, h + w]
   in 2 * minimum sides + w * l * h

solve1 = sum . map wrap

solve2 = sum . map ribbon

solve = solveDay parser solve1 solve2