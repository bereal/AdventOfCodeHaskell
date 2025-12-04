module Year2015.Day03 where

import Common (InputParser (ParsecParser, StringParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, many1)
import Data.Functor (($>))
import Data.List (partition)
import qualified Data.Set as S

up = char '^' $> (0, 1)

down = char 'v' $> (0, -1)

left = char '<' $> (-1, 0)

right = char '>' $> (1, 0)

parser :: InputParser [(Int, Int)]
parser = ParsecParser $ many1 (up <|> down <|> left <|> right)

move (x, y) (dx, dy) = (x + dx, y + dy)

visit = S.fromList . scanl move (0, 0)

solve1 = S.size . visit

evenOdd ls =
  let (evens, odds) = partition fst $ zip (cycle [True, False]) ls
   in (map snd evens, map snd odds)

solve2 program =
  let (santa, robot) = evenOdd program
   in S.size $ S.union (visit santa) (visit robot)

solve = solveDay parser solve1 solve2