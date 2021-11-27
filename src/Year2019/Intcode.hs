{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Intcode where

import Data.IntMap ((!))
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.Printf (printf)

type Memory = IM.IntMap Int

data ProgramState = ProgramState
  { ptr :: Int,
    state :: State,
    memory :: Memory,
    input :: [Int],
    output :: [Int],
    base :: Int,
    name :: String
  }

instance Show ProgramState where
  show ProgramState {name, state, input, ptr, base} =
    printf "Program %s {state %s, input %s, ptr %d, base %d}" name (show state) (show input) ptr base

data State = Running | Halt | Waiting deriving (Eq, Show)

type AddrMode = Int

(!?) :: ProgramState -> Int -> Int
(!?) ProgramState {memory} i = fromMaybe 0 (IM.lookup i memory)

parseCommand :: Int -> (Int, AddrMode, AddrMode, AddrMode)
parseCommand c =
  let (c1, code) = c `divMod` 100
      (c2, mode1) = c1 `divMod` 10
      (mode3, mode2) = c2 `divMod` 10
   in (code, mode1, mode2, mode3)

initProgram :: [Int] -> ProgramState
initProgram p = ProgramState 0 Running (IM.fromAscList $ zip [0 ..] p) [] [] 0 ""

patchMem :: [(Int, Int)] -> ProgramState -> ProgramState
patchMem upd ps@ProgramState {memory} =
  let m' = IM.union (IM.fromList upd) memory in ps {memory = m'}

boolOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
boolOp f a b = if f a b then 1 else 0

runStep :: ProgramState -> ProgramState
runStep s@ProgramState {ptr, memory} = case parseCommand (memory ! ptr) of
  (1, m1, m2, m3) -> binOp (+) (m1, m2, m3) s
  (2, m1, m2, m3) -> binOp (*) (m1, m2, m3) s
  (3, m, _, _) -> readInput m s
  (4, m, _, _) -> writeOutput m s
  (5, m1, m2, _) -> jumpIf (/= 0) (m1, m2) s
  (6, m1, m2, _) -> jumpIf (== 0) (m1, m2) s
  (7, m1, m2, m3) -> binOp (boolOp (<)) (m1, m2, m3) s
  (8, m1, m2, m3) -> binOp (boolOp (==)) (m1, m2, m3) s
  (9, m, _, _) -> setBase m s
  (99, _, _, _) -> s {state = Halt}

evalAddr :: AddrMode -> Int -> ProgramState -> Int
evalAddr 0 v p@ProgramState {ptr} = p !? (ptr + v)
evalAddr 1 v ProgramState {ptr} = ptr + v
evalAddr 2 v p@ProgramState {ptr, base} = base + (p !? (ptr + v))

readInput m p@ProgramState {input = (i : is), ptr, memory} =
  let addr = evalAddr m 1 p
   in p {input = is, ptr = ptr + 2, memory = IM.insert addr i memory}
readInput _ p@ProgramState {input = []} = p {state = Waiting}

writeOutput m p@ProgramState {..} =
  let addr = evalAddr m 1 p
   in p {ptr = ptr + 2, output = p !? addr : output}

jumpIf cond (m1, m2) p@ProgramState {..} =
  let v = p !? evalAddr m1 1 p
      ptr' = if cond v then p !? evalAddr m2 2 p else ptr + 3
   in p {ptr = ptr'}

binOp :: (Int -> Int -> Int) -> (AddrMode, AddrMode, AddrMode) -> ProgramState -> ProgramState
binOp op (m1, m2, m3) p@ProgramState {ptr, memory} =
  let a1 = p !? evalAddr m1 1 p
      a2 = p !? evalAddr m2 2 p
      d = evalAddr m3 3 p
      v = a1 `op` a2
      mem' = IM.insert d v memory
   in p {ptr = ptr + 4, memory = mem'}

setBase m p@ProgramState {memory, ptr, base} =
  let upd = p !? evalAddr m 1 p
   in p {base = base + upd, ptr = ptr + 2}

runProgram :: ProgramState -> ProgramState
runProgram s =
  let s'@ProgramState {ptr, state} = runStep s
   in if state == Running then runProgram s' else s'

runProgramWithArgs :: Int -> Int -> ProgramState -> ProgramState
runProgramWithArgs a b p = let p' = patchMem [(1, a), (2, b)] p in runProgram p'

runProgramWithInput :: [Int] -> ProgramState -> ProgramState
runProgramWithInput i p@ProgramState {input} = runProgram $ p {output = [], input = input ++ i, state = Running}