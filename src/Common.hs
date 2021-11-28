{-# LANGUAGE BangPatterns #-}

module Common where

import Client (ClientConfig, downloadInput)
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text, pack)
import qualified Data.Text.IO as IO
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

data InputParser a = ParsecParser (Parser a) | TextParser (Text -> Either String a) | HardCoded a

data Input = HTTPInput ClientConfig Int Int | FileInput FilePath

type DayRunner = Input -> IO ()

readInput :: Input -> IO Text
readInput (HTTPInput config year day) = downloadInput config year day
readInput (FileInput path) = IO.readFile path

parseInput :: InputParser a -> Text -> a
parseInput parser input =
  let result = case parser of
        (ParsecParser p) -> parseOnly p input
        (TextParser p) -> p input
        (HardCoded a) -> Right a
   in case result of
        Left err -> error err
        Right a -> a

skipDay _ = putStrLn "No solution"

solveDay :: (Show b, Show c) => InputParser a -> (a -> b) -> (a -> c) -> DayRunner
solveDay parser p1 p2 input = do
  d <- readInput input
  let v = parseInput parser d
  start <- getMonotonicTime
  let !r1 = p1 v
  end1 <- getMonotonicTime
  let !r2 = p2 v
  end2 <- getMonotonicTime
  printf "Part 1: %s (%fs)\n" (show r1) (end1 - start)
  printf "Part 2: %s (%fs)\n" (show r2) (end2 - end1)
