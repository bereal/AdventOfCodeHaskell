{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day21 where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text, pack)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (decimal, letter, many1, anyChar, space, string)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))
import Common (solveDay, lineParser)

data Expr = Val Int | Op Text Char Text deriving (Show, Generic, NFData)

name = pack <$> many1 letter
expr = (Val <$> decimal) <|> (Op <$> (name <* space) <*> anyChar <*> (space *> name))
line = (,) <$> (name <* string ": ") <*> expr
parser = HM.fromList <$> lineParser line

type NS = HM.HashMap Text Expr

eval '*' a b = a * b
eval '/' a b = a `div` b
eval '-' a b = a - b
eval '+' a b = a + b

data Equation = Const Int | Solver (Int -> Int)

(.>.) g (Solver f) = Solver $ f . g

equation op  (Const a)  (Const b)  = Const $ eval op a b
equation '*' (Const a)  s          = (`div` a)  .>. s
equation '*' s          (Const a)  = (`div` a)  .>. s
equation '/' (Const a)  s          = (a `div`)  .>. s
equation '/' s          (Const a)  = (a *)      .>. s
equation '+' (Const a)  s          = subtract a .>. s
equation '+' s          (Const a)  = subtract a .>. s
equation '-' (Const a)  s          = (a -)      .>. s
equation '-' s          (Const a)  = (a +)      .>. s
equation '=' (Solver f) (Const a)  = Const $ f a
equation '=' (Const a)  (Solver f) = Const $ f a

buildEquation :: Bool -> NS -> Text -> Equation
buildEquation True _ "humn" = Solver id
buildEquation sec ns n = case ns ! n of
    Val i -> Const i
    Op left op right -> let
        le = buildEquation sec ns left
        re = buildEquation sec ns right
        op' = if sec && n == "root" then '=' else op
     in equation op' le re

solve' sec ns = let (Const a) = buildEquation sec ns "root" in a

solve = solveDay parser (solve' False) (solve' True)