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
import Data.Bits (shift)
import Data.Functor (($>), (<&>))

data Header = Header {version :: Int, typeId :: Int}

data Length = InPackets Int | InBits Int

data Packet = Operator {header :: Header, subPackets :: [Packet]} | Literal {header :: Header, value :: Int}

type BitStream = (Int, [Int])

type BitReader = State BitStream

hexToBits :: String -> [Int]
hexToBits = concatMap bits
  where
    bits '0' = [0, 0, 0, 0]
    bits '1' = [0, 0, 0, 1]
    bits '2' = [0, 0, 1, 0]
    bits '3' = [0, 0, 1, 1]
    bits '4' = [0, 1, 0, 0]
    bits '5' = [0, 1, 0, 1]
    bits '6' = [0, 1, 1, 0]
    bits '7' = [0, 1, 1, 1]
    bits '8' = [1, 0, 0, 0]
    bits '9' = [1, 0, 0, 1]
    bits 'A' = [1, 0, 1, 0]
    bits 'B' = [1, 0, 1, 1]
    bits 'C' = [1, 1, 0, 0]
    bits 'D' = [1, 1, 0, 1]
    bits 'E' = [1, 1, 1, 0]
    bits 'F' = [1, 1, 1, 1]

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
