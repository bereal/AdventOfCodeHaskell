{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day06 where

import Common (Input, InputParser (ParsecParser, TextParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, digit, endOfLine, letter, many1, parse, parseOnly, sepBy1)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Tuple (swap)
import Language.Haskell.TH (Stmt (ParS))

name = many1 $ letter <|> digit

orbit = (,) <$> name <* char ')' <*> name

parser = ParsecParser $ sepBy1 orbit endOfLine

solve1 :: [(String, String)] -> Int
solve1 orbits =
  let m = M.fromListWith (++) (map (\(x, y) -> (x, [y])) orbits)
      count' c node = case M.lookup node m of
        Nothing -> c
        Just ns -> c + sum (map (count' (c + 1)) ns)
   in count' 0 "COM"

pathToNode :: String -> M.Map String String -> [String]
pathToNode = path' []
  where
    path' buf node m = case M.lookup node m of
      Nothing -> node : buf
      Just p -> path' (node : buf) p m

solve2 orbits =
  let m = M.fromList $ map swap orbits
      path1 = pathToNode "SAN" m
      path2 = pathToNode "YOU" m
      dist (x : xs) (y : ys)
        | x == y = dist xs ys
        | otherwise = length xs + length ys
   in dist path1 path2

solve = solveDay parser solve1 solve2
