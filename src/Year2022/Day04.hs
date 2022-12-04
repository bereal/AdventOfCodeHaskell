module Year2022.Day04 where

import Common (InputParser, solveDay, lineParser)
import Data.Attoparsec.Text (Parser, decimal, char)

pair p sep = (,) <$> (p <* char sep) <*> p

parser :: InputParser [((Int, Int), (Int, Int))]
parser = lineParser $ pair range ','
    where range = pair decimal '-'

includes ((a, b), (c, d)) = a <= c && b >= d || a >= c && b <= d

intersects ((a, b), (c, d)) = a <= d && b >= c

count f = length . filter f


solve = solveDay parser (count includes) (count intersects)
