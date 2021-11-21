{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Client (ClientConfig, initOrReadConfig)
import Common (DayRunner, Input (HTTPInput))
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import System.Console.GetOpt (ArgDescr (OptArg), ArgOrder (Permute), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Year2021

years = M.fromList [(2021, Year2021.days)]

run config year day = do
  let yearDays = years ! year
  printf "Running day %d\n" day
  let solve = yearDays !! (day - 1)
   in solve $ HTTPInput config year day

data Flag = Year Int | Day Int | Basedir FilePath deriving (Show)

data ParsedFlags = ParsedFlags {fBasedir :: Maybe FilePath, fYear :: Maybe Int, fDay :: Maybe Int}

flags =
  [ Option ['y'] ["year"] (OptArg ((Year . read) <$>) "YEAR") "year",
    Option ['d'] ["day"] (OptArg ((Day . read) <$>) "DAY") "day",
    Option ['b'] ["basedir"] (OptArg (Basedir <$>) "BASEDIR") "basedir"
  ]

processFlags :: [Flag] -> ParsedFlags
processFlags = foldl update (ParsedFlags Nothing Nothing Nothing)
  where
    update pf f = case f of
      Year y -> pf {fYear = Just y}
      Day d -> pf {fDay = Just d}
      Basedir b -> pf {fBasedir = Just b}

parseFlags :: IO ParsedFlags
parseFlags = do
  args <- getArgs
  case getOpt Permute flags args of
    (o, _, []) -> return $ processFlags $ catMaybes o
    (_, _, errs) -> fail $ concat errs ++ usageInfo "" flags

main = do
  args <- getArgs
  ParsedFlags {fBasedir, fYear, fDay} <- parseFlags
  config <- initOrReadConfig fBasedir
  let year = fromMaybe 2021 fYear
  let days = maybe [1 .. length (years ! year)] (: []) fDay
  mapM_ (run config year) days
