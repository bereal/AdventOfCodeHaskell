{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Year2022.Day11 where
import Text.Regex.TDFA ((=~), AllTextMatches (getAllTextMatches))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.List.Split (splitOn)
import Common (InputParser(StringParser), solveDay)
import qualified Data.IntMap as M
import Data.IntMap ((!))
import Data.List (sort)

getInts :: String -> [Int]
getInts s = map read (getAllTextMatches (s =~ ("[0-9]+" :: String)) :: [String])

getInt = head . getInts

data Expr = Const Int | Old deriving (Show, NFData, Generic)

data Monkey = Monkey {
    items :: [Int],
    operation :: (Char, Expr),
    divisor :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspected :: Int
} deriving (Show, NFData, Generic)

parseOperation :: String -> (Char, Expr)
parseOperation s = f (s =~ ("(\\*|\\+) (old|[0-9]+)" :: String) :: (String, String, String, [String])) where
    f (_, _, _, [op, "old"]) = (head op, Old)
    f (_, _, _, [op, i]) = (head op, Const $ read i)

parseMonkey :: [String] -> Monkey
parseMonkey [_, itemsS, opS, divS, trueS, falseS] = Monkey
    (getInts itemsS)
    (parseOperation opS)
    (getInt divS)
    (getInt trueS)
    (getInt falseS)
    0

parser :: InputParser [Monkey]
parser = StringParser $ Right . map (parseMonkey . lines) . splitOn "\n\n"

type State = M.IntMap Monkey

eraseItems :: Monkey -> Monkey
eraseItems m@Monkey{..} = m {items=[], inspected=inspected + length items}

evalExpr :: Int -> Expr -> Int
evalExpr i Old = i
evalExpr _ (Const j) = j

evalOp :: Int -> (Char, Expr) -> Int
evalOp i ('*', exp) = i * evalExpr i exp
evalOp i ('+', exp) = i + evalExpr i exp

addItem :: Int -> Monkey -> Monkey
addItem i m@Monkey{..} = m {items=items ++ [i]}

processItem :: (Int -> Int) -> Monkey -> State -> Int -> State
processItem reduce m@Monkey{..} st i = let
    new = reduce $ i `evalOp` operation
    target = if new `mod` divisor == 0 then ifTrue else ifFalse
  in M.adjust (addItem new) target st

processMonkey :: (Int -> Int) ->  State -> Int -> State
processMonkey reduce st i = let
    m@Monkey{..} = st ! i
    st' = foldl (processItem reduce m) st items
  in M.adjust eraseItems i st'

processRound :: (Int -> Int) -> State -> State
processRound reduce st = foldl (processMonkey reduce) st $ M.keys st

solve' reduce rounds = product . take 2 . reverse . sort . map inspected .
    M.elems . (!!rounds) . iterate (processRound reduce) . M.fromAscList . zip [0..]

solve1 = solve' (`div` 3) 20
solve2 monkeys = let m = foldl1 lcm $ map divisor monkeys
    in solve' (`mod` m) 10000 monkeys

solve = solveDay parser solve1 solve2