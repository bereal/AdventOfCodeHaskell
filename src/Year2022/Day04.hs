module Year2022.Day04 where

import Common (solveDay, lineParser)
import Data.Attoparsec.Text (Parser, decimal, char)

range :: Parser (Int, Int)
range = (,) <$> decimal <*> (char '-' *> decimal)

parser = lineParser $ (,) <$> range <*> (char ',' *> range)

includes ((a, b), (c, d)) = a <= c && b >= d || a >= c && b <= d

intersects ((a, b), (c, d)) = a <= d && b >= c

count f = length . filter f

solve = solveDay parser (count includes) (count intersects)