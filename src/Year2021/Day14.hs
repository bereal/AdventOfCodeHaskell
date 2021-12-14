{-# LANGUAGE TupleSections #-}

module Year2021.Day14 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Control.Arrow (first)
import Data.Attoparsec.Text (anyChar, endOfInput, endOfLine, letter, many1, sepBy1, string)
import Data.List (group, sort)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Debug.Trace (trace)

type Pair = (Char, Char)

type Counter = M.Map Pair Int

type Rules = M.Map Pair [Pair]

convertTemplate :: String -> Counter
convertTemplate t = M.fromListWith (+) $ map (,1) (zip (' ' : t) t)

convertRule ((a, b), c) = ((a, b), [(a, c), (c, b)])

template = convertTemplate <$> many1 letter

lhs = (,) <$> letter <*> letter

rule = (,) <$> (lhs <* string " -> ") <*> letter

rules = M.fromList . map convertRule <$> sepBy1 rule endOfLine

parser = ParsecParser $ (,) <$> template <*> (endOfLine *> endOfLine *> rules)

step :: Rules -> Counter -> Counter
step m counter =
  let resolve (p, c) = case m !? p of
        Just ps -> map (,c) ps
        Nothing -> [(p, c)]
   in M.fromListWith (+) $ concatMap resolve $ M.toList counter

solve' :: Int -> (Counter, Rules) -> Int
solve' n (template, rules) =
  let result = iterate (step rules) template !! n
      letters = M.fromListWith (+) $ map (first snd) $ M.toList result
      counts = sort $ map snd $ M.toList letters
   in last counts - head counts

solve = solveDay parser (solve' 10) (solve' 40)
