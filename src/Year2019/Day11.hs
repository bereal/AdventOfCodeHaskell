{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day11 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (anyChar, char, decimal, many1, sepBy1, signed)
import Data.List (intercalate)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Year2019.Intcode (ProgramState (ProgramState, output, state), State (Halt), initProgram, runProgramWithInput)

parser = ParsecParser $ initProgram <$> sepBy1 (signed decimal) (char ',')

data Direction = W | N | E | S deriving (Show, Enum)

type Coord = (Int, Int)

type Floor = M.Map Coord Int

data Position = Pos Coord Direction

paint :: Floor -> Image
paint f =
  let (a, b, c, d) = foldl (\(a, b, c, d) (x, y) -> (min a x, min b y, max a x, max d y)) (0, 0, 0, 0) $ M.keys f
      cell x = maybe ' ' ([' ', '*'] !!) (f !? x)
      row r = map (cell . (,r)) [a .. c]
   in Image $ "\n" ++ intercalate "\n" (map row [b .. d])

newtype Image = Image String

instance Show Image where
  show (Image s) = s

turn 0 W = S
turn 1 S = W
turn 0 x = pred x
turn 1 x = succ x

move :: Direction -> Coord -> Coord
move W (x, y) = (x - 1, y)
move N (x, y) = (x, y - 1)
move E (x, y) = (x + 1, y)
move S (x, y) = (x, y + 1)

updatePos :: Int -> Position -> Position
updatePos i (Pos c d) = let d' = turn i d in Pos (move d' c) d'

step :: Position -> Floor -> ProgramState -> (Position, Floor, ProgramState)
step pos@(Pos coord _) floor p =
  let color = fromMaybe 0 (floor !? coord)
      p'@ProgramState {output} = runProgramWithInput [color] p
      updatedState [] = (pos, floor)
      updatedState [d, c] = (updatePos d pos, M.insert coord c floor)
      updatedState x = error $ show x
      (pos', floor') = updatedState output
   in (pos', floor', p')

run color = run' (Pos (0, 0) N) (M.singleton (0, 0) color)
  where
    run' _ floor p@ProgramState {state = Halt} = floor
    run' pos floor p = let (pos', floor', p') = step pos floor p in run' pos' floor' p'

solve1 = M.size . run 0

solve2 = paint . run 1

solve = solveDay parser solve1 solve2
