{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day16 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    gets,
    replicateM,
    when,
  )
import Data.Attoparsec.Text (digit, letter, many1)
import Data.Bits (shift, (.&.))
import Data.Char (digitToInt)
import Data.Functor (($>))

data Header = Header {version :: Int, typeId :: Int}

data Length = InPackets Int | InBits Int

data Packet = Operator {header :: Header, subPackets :: [Packet]} | Literal {header :: Header, value :: Int}

type BitStream = (Int, [Int])

type BitReader = State BitStream

hexToBits :: String -> [Int]
hexToBits = concatMap bits
  where
    bit n i = (n `shift` (- i)) .&. 1
    bits n = map (bit $ digitToInt n) [3, 2, 1, 0]

bitsToInt :: [Int] -> Int
bitsToInt = foldl ((+) . (`shift` 1)) 0

nibblesToInt :: [Int] -> Int
nibblesToInt = foldl ((+) . (`shift` 4)) 0

endOfStream :: BitReader Bool
endOfStream = gets ((< 4) . length . snd)

readBits :: Int -> BitReader [Int]
readBits len = do
  (offset, bits) <- get
  let (h, t) = splitAt len bits
  put (offset + len, t)
  return h

readInt :: Int -> BitReader Int
readInt len = bitsToInt <$> readBits len

readBool :: BitReader Bool
readBool = toEnum <$> readInt 1

readHeader :: BitReader Header
readHeader = Header <$> readInt 3 <*> readInt 3

readLength :: BitReader Length
readLength = do
  inPackets <- readBool
  if inPackets then InPackets <$> readInt 11 else InBits <$> readInt 15

readLiteral :: BitReader Int
readLiteral = nibblesToInt <$> read'
  where
    read' = do
      keepReading <- readBool
      s <- get
      val <- readInt 4
      if keepReading then (val :) <$> read' else return [val]

readSubPackets :: Length -> BitReader [Packet]
readSubPackets (InPackets ps) = replicateM ps readPacket
readSubPackets (InBits bs) = evalStream <$> readBits bs

readOperator :: BitReader [Packet]
readOperator = do
  len <- readLength
  readSubPackets len

readPacket :: BitReader Packet
readPacket = do
  header@Header {..} <- readHeader
  if typeId == 4 then Literal header <$> readLiteral else Operator header <$> readOperator

readStream :: BitReader [Packet]
readStream = do
  end <- endOfStream
  if end then return [] else (:) <$> readPacket <*> readStream

evalStream :: [Int] -> [Packet]
evalStream bits = evalState readStream (0, bits)

evalPacket :: Packet -> Int
evalPacket Literal {value} = value
evalPacket Operator {header, subPackets} =
  let subVals = map evalPacket subPackets
      compare op = fromEnum $ head subVals `op` (subVals !! 1)
   in case typeId header of
        0 -> sum subVals
        1 -> product subVals
        2 -> minimum subVals
        3 -> maximum subVals
        5 -> compare (>)
        6 -> compare (<)
        7 -> compare (==)

parser :: InputParser [Int]
parser = ParsecParser $ hexToBits <$> many1 (letter <|> digit)

listVersions :: Packet -> [Int]
listVersions Literal {header} = [version header]
listVersions Operator {header, subPackets} = version header : concatMap listVersions subPackets

solve1 bits = sum $ concatMap listVersions $ evalStream bits

solve2 = evalPacket . head . evalStream

solve = solveDay parser solve1 solve2
