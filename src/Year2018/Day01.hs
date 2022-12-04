{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2018.Day01 where
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (signed, decimal)
import qualified Data.IntSet as S

parser = lineParser $ signed decimal

solve2 = find S.empty . scanl1 (+) . cycle where
    find s (x:xs)
        | S.member x s = x
        | otherwise = find (S.insert x s) xs

solve = solveDay parser sum solve2