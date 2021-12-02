module Year2021.Day02 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy1, space, string)
import Data.Text (Text)

data Command = Depth Int | Forward Int

cmd name = string name *> space *> decimal

up = Depth . negate <$> cmd "up"

down = Depth <$> cmd "down"

forward = Forward <$> cmd "forward"

parser = ParsecParser $ sepBy1 (up <|> down <|> forward) endOfLine

move1 (h, v) (Depth i) = (h, v + i)
move1 (h, v) (Forward i) = (h + i, v)

solve1 = uncurry (*) . foldl move1 (0, 0)

move2 (h, v, a) (Depth i) = (h, v, a + i)
move2 (h, v, a) (Forward i) = (h + i, v + a * i, a)

solve2 steps = h * v where (h, v, _) = foldl move2 (0, 0, 0) steps

solve = solveDay parser solve1 solve2
