{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day24 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (anyChar, char, decimal, endOfInput, endOfLine, many1, sepBy1, signed, space, string)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Tuple.All (sel1, sel2, sel3, sel4, upd1, upd2, upd3, upd4)

type ProgramState = (Memory, [Int])

type Memory = (Int, Int, Int, Int)

type Var = (Memory -> Int, Int -> Memory -> Memory)

data Value = Var Var | Literal Int

w, x, y, z :: Var
w = (sel1, upd1)
x = (sel2, upd2)
y = (sel3, upd3)
z = (sel4, upd4)

eval :: Value -> Memory -> Int
eval (Var (get, _)) m = get m
eval (Literal v) _ = v

binOp :: (Int -> Int -> Int) -> Var -> Value -> ProgramState -> ProgramState
binOp op (get, put) v (m, i) = let m' = put (get m `op` eval v m) m in (m', i)

input :: Var -> ProgramState -> ProgramState
input (_, put) (m, h : t) = let m' = put h m in (m', t)

var = (char 'w' $> w) <|> (char 'x' $> x) <|> (char 'y' $> y) <|> (char 'z' $> z)

value = (Var <$> var) <|> (Literal <$> signed decimal)

binOpParser s op = binOp op <$> (string s *> space *> var) <*> (space *> value)

inputParser = string "inp" *> space *> (input <$> var)

cmdParser =
  inputParser
    <|> binOpParser "add" (+)
    <|> binOpParser "mul" (*)
    <|> binOpParser "mod" mod
    <|> binOpParser "div" div
    <|> binOpParser "eql" ((fromEnum .) . (==))

parser = ParsecParser $ sepBy1 cmdParser endOfLine

runProgram input prog =
  let ps = ((0, 0, 0, 0), input)
      (m, _) = foldl (flip ($)) ps prog
   in fst z m

solve = solveDay parser (runProgram $ replicate 14 1) (\a -> "a" :: String)
