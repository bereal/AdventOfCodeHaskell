{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Year2022.Day13 where
import Data.Attoparsec.Text (char, string, sepBy, decimal, endOfLine)
import Control.Applicative
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Bifunctor (second)
import Data.List (sort, findIndex, findIndices)
import Common (InputParser (ParsecParser), solveDay)

data Packet = List [Packet] | Val Int deriving (Eq, Show, NFData, Generic)

packetList = List <$> (char '[' *> (packet `sepBy` char ',') <* char ']')
packet = packetList <|> Val <$> decimal

pair = (,) <$> (packet <* endOfLine) <*> packet

parser = ParsecParser $ pair `sepBy` (endOfLine *> endOfLine)

instance Ord Packet where
    compare (List []) (List []) = EQ
    compare (List []) (List b) = LT
    compare (List (x:xs)) (List []) = GT
    compare (List (x:xs)) (List (y:ys)) = case compare x y of
        EQ -> compare (List xs) (List ys)
        v -> v
    compare a@(List _) b = compare a $ List [b]
    compare a b@(List _) = compare (List [a]) b
    compare (Val a) (Val b) = if | a < b -> LT
                                 | a > b -> GT
                                 | otherwise -> EQ

solve1 :: [(Packet, Packet)] -> Int
solve1 = sum . map fst . filter (snd . second (uncurry (<))) . zip [1..]

divider1 = List [List [Val 2]]
divider2 = List [List [Val 6]]
solve2 pairs = let packets = [divider1, divider2] ++ concatMap (\(a, b) -> [a, b]) pairs
                   ids = findIndices (\a -> a == divider1 || a == divider2) $ sort packets
                in product $ map succ ids

solve = solveDay parser solve1 solve2