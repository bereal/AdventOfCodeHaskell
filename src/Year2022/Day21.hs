{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day21 where
import Common (solveDay, lineParser)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))
import Data.Text
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (decimal, letter, many1, anyChar, space, string)

data Expr = Val Int | Op Text Char Text deriving (Show, Generic, NFData)

name = pack <$> many1 letter
expr = (Val <$> decimal) <|> (Op <$> (name <* space) <*> anyChar <*> (space *> name))
line = (,) <$> (name <* string ": ") <*> expr
parser = HM.fromList <$> lineParser line

type NS = HM.HashMap Text Expr

evalOp '*' a b = a * b
evalOp '/' a b = a `div` b
evalOp '-' a b = a - b
evalOp '+' a b = a + b

eval :: Text -> NS -> Int
eval n ns = case ns ! n of
    Val i -> i
    Op left op right -> let
        lv = eval left ns
        rv = eval right ns
        v = evalOp op lv rv
      in v

data Equation = Const Int | Solver (Int -> Int)

(.>.) g (Solver f) = Solver $ f . g

equation op (Const a) (Const b)     = Const $ evalOp op a b
equation '*' (Const a) s            = (`div` a)  .>. s
equation '*' s (Const a)            = (`div` a)  .>. s
equation '/' (Const a) s            = (a `div`)  .>. s
equation '/' s@(Solver f) (Const a) = (a *)      .>. s
equation '+' (Const a) s            = subtract a .>. s
equation '+' s (Const a)            = subtract a .>. s
equation '-' (Const a) s            = (a -)      .>. s
equation '-' s (Const a)            = (a +)      .>. s
equation '=' (Solver f) (Const a)   = Const $ f a
equation '=' (Const a) (Solver f)   = Const $ f a

buildEquation :: NS -> Text -> Equation
buildEquation _ "humn" = Solver id
buildEquation ns n = case ns ! n of
    Val i -> Const i
    Op left op right -> let
        le = buildEquation ns left
        re = buildEquation ns right
        op' = if n == "root" then '=' else op
     in equation op' le re

solve1 = eval "root"

solve2 ns = let (Const a) = buildEquation ns "root" in a

solve = solveDay parser solve1 solve2