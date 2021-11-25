{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day07 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1)
import Data.List (permutations)
import Data.Text (Text)
import Debug.Trace (trace)
import Year2019.Intcode
  ( ProgramState (ProgramState, input, output, state),
    State (Halt, Waiting),
    initProgram,
    runProgramWithInput,
  )

parser = ParsecParser $ initProgram <$> sepBy1 decimal (char ',')

runAmp :: ProgramState -> [Int] -> ProgramState
runAmp p init = runProgramWithInput init p

runSeq = runSeq' 0
  where
    runSeq' init prog (x : xs) =
      let ProgramState {output = (init' : _)} = runAmp prog [x, init]
       in runSeq' init' prog xs
    runSeq' init prog [] = init

runLoop prog seq =
  let runLoop' (p : ps) init = case runAmp p [init] of
        ProgramState {state = Halt, output = (i : _)} -> runLoop' ps i
        p'@ProgramState {state = Waiting, output = (i : _)} -> runLoop' (ps ++ [p']) i
      runLoop' [] init = init
      progs = map (\a -> prog {input = [a]}) seq
   in runLoop' progs 0

solve1 prog = maximum $ map (runSeq prog) (permutations [0 .. 4])

solve2 prog = map (runLoop prog) (permutations [5 .. 9])

solve = solveDay parser solve1 solve2
