{-# LANGUAGE TupleSections #-}
module Year2022.Day09 where
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (string, decimal)
import Control.Applicative ((<|>))
import qualified Data.Set as S

type Vec2 = (Int, Int)

left = (,0) . negate <$> (string "L " *> decimal)
right = (,0) <$> (string "R " *> decimal)
up = (0,) <$> (string "U " *> decimal)
down = (0,) . negate <$> (string "D " *> decimal)

parser = lineParser (left <|> right <|> up <|> down)

-- convert [(3, 0), (0, -2)] to [(1, 0), (1, 0), (1, 0), (0, -1), (0, -1)]
unfold :: [Vec2] -> [Vec2]
unfold = concatMap uf where
    uf (a, 0) = replicate (abs a) (signum a, 0)
    uf (_, a) = replicate (abs a) (0, signum a)

direction (tx, ty) (hx, hy) = let
    dx = hx - tx
    dy = hy - ty
  in if max (abs dx) (abs dy) <= 1 then (0, 0) else (signum dx, signum dy)

add :: Vec2 -> Vec2 -> Vec2
add (a, b) (c, d) = (a + c, b + d)

-- move the head and pull the tail one by one
move (h:t@(x:_)) d = let
    h' = add h d
    d' = direction x h'
  in h' : move t d'
move [h] d = [add h d]
move [] _ = []

solve' n path = let rope = replicate n (0, 0)
    in S.size $ S.fromList $ map last $ scanl move rope $ unfold path

solve = solveDay parser (solve' 2) (solve' 10)