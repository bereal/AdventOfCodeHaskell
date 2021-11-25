{-# LANGUAGE NamedFieldPuns #-}

module Year2019.Day02 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)
import Data.Text (Text)
import Data.Vector ((!))
import Debug.Trace
import Year2019.Intcode (ProgramState (ProgramState, memory, ptr), initProgram, patchMem, runProgramWithArgs)

parser = ParsecParser $ sepBy1 decimal (char ',')

getResult a b p = let ProgramState {memory} = runProgramWithArgs a b p in memory ! 0

solve1 p =
  let s = initProgram p in getResult 12 2 s

solve2 p =
  let s = initProgram p
      seeds = [(a, b) | a <- [0 .. 99], b <- [0 .. 99]]
      matches (a, b) = getResult a b s == 19690720
      (a, b) = head $ filter matches seeds
   in 100 * a + b

solve = solveDay parser solve1 solve2
