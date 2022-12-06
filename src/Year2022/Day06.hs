module Year2022.Day06 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (many1, letter)
import Data.List (nub, tails, findIndex)
import Data.Maybe (fromJust)

slide n = map (take n) . tails

solve' :: Int -> String -> Int
solve' n = (+n) . fromJust . findIndex ((==n) . length . nub) . slide n

solve = solveDay (ParsecParser $ many1 letter) (solve' 4) (solve' 14)