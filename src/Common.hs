{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import Client (ClientConfig, downloadInput)
import Control.DeepSeq (NFData, deepseq)
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text, pack)
import qualified Data.Text.IO as IO
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import Text.Printf (printf)

data InputParser a = ParsecParser (Parser a) | TextParser (Text -> Either String a) | HardCoded a

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
   in case result of
        Left err -> error err
        Right a -> a

skipDay _ = putStrLn "No solution"

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
