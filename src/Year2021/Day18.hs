{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Year2021.Day18 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (char, decimal, endOfLine, many1, sepBy1)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Text.Printf (printf)

data SnailfishNumber = Regular Int | Pair SnailfishNumber SnailfishNumber deriving (Generic, NFData)

instance Show SnailfishNumber where
  show (Regular i) = show i
  show (Pair l r) = printf "[%s,%s]" (show l) (show r)

pair = Pair <$> (char '[' *> snailfishNumber) <*> (char ',' *> snailfishNumber <* char ']')

snailfishNumber = (Regular <$> decimal) <|> pair

parser = ParsecParser $ sepBy1 snailfishNumber endOfLine

data Side = L | R

data ExplosionResult = Overflow Side Int SnailfishNumber | Replace SnailfishNumber | NoExplosion

-- Add a number to the outmost leaf node of a tree
addToLeaf :: Side -> Int -> SnailfishNumber -> SnailfishNumber
addToLeaf _ i (Regular j) = Regular $ i + j
addToLeaf R i (Pair l r) = Pair l $ addToLeaf R i r
addToLeaf L i (Pair l r) = Pair (addToLeaf L i l) r

-- Return a new number if an explosion happened
explode :: SnailfishNumber -> Maybe SnailfishNumber
explode = toMaybe . explode' 0
  where
    toMaybe (Overflow _ _ n) = Just n
    toMaybe (Replace n) = Just n
    toMaybe _ = Nothing
    explode' _ (Regular _) = NoExplosion
    explode' 3 (Pair (Pair (Regular a) (Regular b)) r) = Overflow L a $ Pair (Regular 0) (addToLeaf L b r)
    explode' 3 (Pair l (Pair (Regular a) (Regular b))) = Overflow R b $ Pair (addToLeaf R a l) (Regular 0)
    explode' n (Pair l r) = case (explode' (n + 1) l, explode' (n + 1) r) of
      (Overflow L i l', _) -> Overflow L i $ Pair l' r
      (Overflow R i l', _) -> Replace $ Pair l' $ addToLeaf L i r
      (Replace l', _) -> Replace $ Pair l' r
      (_, Overflow L i r') -> Replace $ Pair (addToLeaf R i l) r'
      (_, Overflow R i r') -> Overflow R i $ Pair l r'
      (_, Replace r') -> Replace $ Pair l r'
      _ -> NoExplosion

-- Return a new number if a split happened
split :: SnailfishNumber -> Maybe SnailfishNumber
split (Regular i)
  | i > 9 = let (l, m) = i `divMod` 2 in Just $ Pair (Regular l) (Regular $ l + m)
  | otherwise = Nothing
split (Pair l r) = case (split l, split r) of
  (Just l', _) -> Just $ Pair l' r
  (_, Just r') -> Just $ Pair l r'
  _ -> Nothing

reduce :: SnailfishNumber -> SnailfishNumber
reduce sn = maybe sn reduce (explode sn <|> split sn)

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add a b = reduce $ Pair a b

magnitude :: SnailfishNumber -> Int
magnitude (Regular i) = i
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

pairs :: [a] -> [(a, a)]
pairs as = [(a, b) | (a, i) <- zip as [1 ..], (b, j) <- zip as [1 ..], i /= j]

solve1 :: [SnailfishNumber] -> Int
solve1 = magnitude . foldl1 add

solve2 = maximum . map (magnitude . uncurry add) . pairs

solve = solveDay parser solve1 solve2
