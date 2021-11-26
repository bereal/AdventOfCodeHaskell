{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Intcode where

import qualified Data.IntMap as IM
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Debug.Trace (trace)
import Text.Printf (printf)

data ProgramState = ProgramState
  { ptr :: Int,
    state :: State,
    memory :: V.Vector Int,
    input :: [Int],
    output :: [Int],
    name :: String
  }

instance Show ProgramState where
  show ProgramState {name, state, input, ptr} = printf "Program %s {state %s, input %s, ptr %d}" name (show state) (show input) ptr

data State = Running | Halt | Waiting deriving (Eq, Show)

type AddrMode = Int

parseCommand :: Int -> (Int, AddrMode, AddrMode, AddrMode)
parseCommand c =
  let (c1, code) = c `divMod` 100
      (c2, mode1) = c1 `divMod` 10
      (c3, mode2) = c2 `divMod` 10
      mode3 = c3 `div` 10
   in (code, mode1, mode2, mode3)

initProgram :: [Int] -> ProgramState
initProgram p = ProgramState 0 Running (V.fromList p) [] [] ""

patchMem :: [(Int, Int)] -> ProgramState -> ProgramState
patchMem upd ps@ProgramState {memory} =
  let m' = memory // upd in ps {memory = m'}

runStep :: ProgramState -> ProgramState
runStep s@ProgramState {ptr, memory} = case parseCommand (memory ! ptr) of
  (1, m1, m2, m3) -> binOp (+) (m1, m2, m3) s
  (2, m1, m2, m3) -> binOp (*) (m1, m2, m3) s
  (3, _, _, _) -> readInput s
  (4, m, _, _) -> writeOutput m s
  (5, m1, m2, _) -> jumpIf (/= 0) (m1, m2) s
  (6, m1, m2, _) -> jumpIf (== 0) (m1, m2) s
  (7, m1, m2, _) -> check (<) (m1, m2) s
  (8, m1, m2, _) -> check (==) (m1, m2) s
  (99, _, _, _) -> s {state = Halt}

evalArg :: AddrMode -> Int -> ProgramState -> Int
evalArg 0 v ProgramState {memory, ptr} = memory ! (memory ! (ptr + v))
evalArg 1 v ProgramState {memory, ptr} = memory ! (ptr + v)

readInput p@ProgramState {input = (i : is), ptr, memory} =
  let addr = evalArg 1 1 p
   in p {input = is, ptr = ptr + 2, memory = memory // [(addr, i)]}
readInput p@ProgramState {input = []} = p {state = Waiting}

writeOutput m p@ProgramState {..} =
  let addr = evalArg 1 1 p
   in p {ptr = ptr + 2, output = (memory ! addr) : output}

jumpIf cond (m1, m2) p@ProgramState {..} =
  let v = evalArg m1 1 p
      ptr' = if cond v then evalArg m2 2 p else ptr + 3
   in p {ptr = ptr'}

check cond (m1, m2) p@ProgramState {..} =
  let v1 = evalArg m1 1 p
      v2 = evalArg m2 2 p
      addr = evalArg 1 3 p
      v = if v1 `cond` v2 then 1 else 0
   in p {ptr = ptr + 4, memory = memory // [(addr, v)]}

binOp :: (Int -> Int -> Int) -> (AddrMode, AddrMode, AddrMode) -> ProgramState -> ProgramState
binOp op (m1, m2, _) p@ProgramState {ptr, memory} =
  let a1 = evalArg m1 1 p
      a2 = evalArg m2 2 p
      d = evalArg 1 3 p
      mem' = memory // [(d, a1 `op` a2)]
   in p {ptr = ptr + 4, memory = mem'}

runProgram :: ProgramState -> ProgramState
runProgram s =
  let s'@ProgramState {ptr, state} = runStep s
   in if state == Running then runProgram s' else s'

runProgramWithArgs :: Int -> Int -> ProgramState -> ProgramState
runProgramWithArgs a b p = let p' = patchMem [(1, a), (2, b)] p in runProgram p'

runProgramWithInput :: [Int] -> ProgramState -> ProgramState
runProgramWithInput i p@ProgramState {input} = runProgram $ p {input = input ++ i, state = Running}