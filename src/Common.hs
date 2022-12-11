{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import Client (ClientConfig, downloadInput)
import Control.DeepSeq (NFData, deepseq)
import Data.Attoparsec.Text (Parser, parseOnly, sepBy1, endOfLine)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as IO
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import Text.Printf (printf)

data InputParser a = ParsecParser (Parser a) | TextParser (Text -> Either String a) | HardCoded a | StringParser (String -> Either String a)

instance Functor InputParser where
  fmap f (ParsecParser p) = ParsecParser $ f <$> p
  fmap f (TextParser p) = TextParser (fmap f . p)
  fmap f (HardCoded a) = HardCoded $ f a
  fmap f (StringParser p) = StringParser(fmap f . p)

data Input = HTTPInput ClientConfig Int Int | FileInput FilePath

newtype ShowAsIs = ShowAsIs String deriving (NFData, Generic)

instance Show ShowAsIs where
  show (ShowAsIs s) = s

type DayRunner = Input -> IO ()

readInput :: Input -> IO Text
readInput (HTTPInput config year day) = downloadInput config year day
readInput (FileInput path) = IO.readFile path

parseInput :: NFData a => InputParser a -> Text -> a
parseInput parser input =
  let result = case parser of
        (ParsecParser p) -> parseOnly p input
        (TextParser p) -> p input
        (HardCoded a) -> Right a
        (StringParser p) -> p $ unpack input
   in case result of
        Left err -> error err
        Right a -> a

lineParser :: Parser a -> InputParser [a]
lineParser p = ParsecParser $ sepBy1 p endOfLine

skipDay _ = putStrLn "No solution"

skipPart = const (ShowAsIs "No solution")

solveDay :: (Show b, Show c, NFData a, NFData b, NFData c) => InputParser a -> (a -> b) -> (a -> c) -> DayRunner
solveDay parser p1 p2 input = do
  d <- readInput input
  parseStart <- getMonotonicTime
  let v = parseInput parser d
  start <- v `deepseq` getMonotonicTime
  printf "Parsed: %fs\n" $ start - parseStart
  let r1 = p1 v
  end1 <- r1 `deepseq` getMonotonicTime
  printf "Part 1: %s (%fs)\n" (show r1) (end1 - start)
  let r2 = p2 v
  end2 <- r2 `deepseq` getMonotonicTime
  printf "Part 2: %s (%fs)\n" (show r2) (end2 - end1)
