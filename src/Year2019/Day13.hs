{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day13 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, many1, sepBy1, signed)
import qualified Data.Map as M
import Data.Text (Text)
import Year2019.Intcode (ProgramState (ProgramState, output, state), State (Halt), initProgram, patchMem, runProgramWithInput)

parser = ParsecParser $ initProgram <$> sepBy1 (signed decimal) (char ',')

type Coord = (Int, Int)

data Screen = Screen {screen :: M.Map Coord Int, ball :: Coord, paddle :: Coord, score :: Int}

processOutput :: Screen -> [Int] -> Screen
processOutput s [] = s
processOutput s (-1 : 0 : s' : as) = processOutput (s {score = s'}) as
processOutput (Screen m b p s) (x : y : c : as) =
  let m' = M.insert (x, y) c m
      b' = if c == 4 then (x, y) else b
      p' = if c == 3 then (x, y) else p
   in processOutput (Screen m' b' p' s) as

emptyScreen = Screen M.empty (0, 0) (0, 0) 0

initGame prog =
  let p@ProgramState {output} = runProgramWithInput [] prog
      s = processOutput emptyScreen $ reverse output
   in (p, s)

solve1 prog =
  let (_, Screen {screen}) = initGame prog
   in M.size $ M.filter (== 2) screen

play prog screen input =
  let p@ProgramState {output, state} = runProgramWithInput input prog
      screen'@Screen {score, ball = (bx, _), paddle = (px, _)} = processOutput screen $ reverse output
   in case state of
        Halt -> score
        _ -> play p screen' [signum (bx - px)]

solve2 prog =
  let (prog', screen) = initGame (patchMem [(0, 2)] prog)
   in play prog' screen []

solve = solveDay parser solve1 solve2
