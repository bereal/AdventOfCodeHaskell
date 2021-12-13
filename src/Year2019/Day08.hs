{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day08 where

import Common (InputParser (ParsecParser, TextParser), ShowAsIs (ShowAsIs), solveDay)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (digit, many1)
import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)

width = 25

height = 6

type Image = [Int]

showImage pixels = ShowAsIs $ '\n' : (intercalate "\n" $ map (map showPixel) $ chunksOf width pixels) ++ "\n"
  where
    showPixel 0 = ' '
    showPixel 1 = '*'

parser = ParsecParser $ chunksOf (width * height) <$> many1 (digitToInt <$> digit)

getLayerStat = foldl f (0, 0, 0)
  where
    f (z, o, t) 0 = (z + 1, o, t)
    f (z, o, t) 1 = (z, o + 1, t)
    f (z, o, t) 2 = (z, o, t + 1)

mergePixels 0 _ = 0
mergePixels 1 _ = 1
mergePixels 2 c = c

mergeLayers = zipWith mergePixels

solve1 :: [[Int]] -> Int
solve1 ls =
  let stats = map getLayerStat ls
      (z, o, t) = minimum stats
   in o * t

solve2 = showImage . foldl1 mergeLayers

solve = solveDay parser solve1 solve2
