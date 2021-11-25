{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Intcode where

import qualified Data.IntMap as IM
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Debug.Trace (trace)

data ProgramState = ProgramState {ptr :: Int, halted :: Bool, memory :: V.Vector Int}

initProgram :: [Int] -> ProgramState
initProgram p = ProgramState 0 False $ V.fromList p

patchMem :: [(Int, Int)] -> ProgramState -> ProgramState
patchMem upd ps@ProgramState {memory} =
  let m' = memory // upd in ps {memory = m'}

runStep :: ProgramState -> ProgramState
runStep s@ProgramState {ptr, memory} = case memory ! ptr of
  1 -> binOp (+) s
  2 -> binOp (*) s
  99 -> s {halted = True}

binOp :: (Int -> Int -> Int) -> ProgramState -> ProgramState
binOp op s@ProgramState {ptr, memory} =
  let a1 = memory ! (memory ! (ptr + 1))
      a2 = memory ! (memory ! (ptr + 2))
      d = memory ! (ptr + 3)
      mem' = memory // [(d, a1 `op` a2)]
   in s {ptr = ptr + 4, memory = mem'}

runProgram :: ProgramState -> Int
runProgram s =
  let s'@ProgramState {halted, memory} = runStep s in if halted then memory ! 0 else runProgram s'

runProgramWithArgs :: Int -> Int -> ProgramState -> Int
runProgramWithArgs a b p = let p' = patchMem [(1, a), (2, b)] p in runProgram p'