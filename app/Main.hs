{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Client (ClientConfig, initOrReadConfig)
import Common (DayRunner, Input (FileInput, HTTPInput))
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Options.Applicative
  ( Parser,
    ReadM,
    auto,
    eitherReader,
    execParser,
    fullDesc,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    short,
    strOption,
    value,
    (<**>),
  )
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readEither)
import qualified Year2016
import qualified Year2018
import qualified Year2019
import qualified Year2020
import qualified Year2021
import qualified Year2022

years =
  M.fromList
    [ (2016, Year2016.days),
      (2018, Year2018.days),
      (2019, Year2019.days),
      (2020, Year2020.days),
      (2021, Year2021.days),
      (2022, Year2022.days)
    ]

run config inputPath year day = do
  let yearDays = years ! year
  printf "== Day %02d ==========\n\n" day
  let solve = yearDays !! (day - 1)
      input = maybe (HTTPInput config year day) FileInput inputPath
   in solve input
  putStrLn "\n===================="

data ParsedFlags = ParsedFlags
  { fBasedir :: Maybe FilePath,
    fYear :: Int,
    fDay :: Maybe Int,
    fInput :: Maybe String
  }

optionalReader :: Read a => ReadM (Maybe a)
optionalReader = eitherReader (fmap Just . readEither)

flagParser :: Parser ParsedFlags
flagParser =
  ParsedFlags
    <$> optional
      ( strOption (short 'b' <> long "basedir" <> metavar "DIRECTORY")
      )
      <*> option auto (short 'y' <> long "year" <> value 2021 <> metavar "YEAR")
      <*> option optionalReader (short 'd' <> long "day" <> value Nothing <> metavar "DAY")
      <*> optional (strOption (short 'i' <> long "input" <> metavar "INPUT_FILE"))

parseFlags = do
  args <- getArgs
  execParser $ info (flagParser <**> helper) fullDesc

main = do
  args <- getArgs
  ParsedFlags {fBasedir, fYear, fDay, fInput} <- parseFlags
  config <- initOrReadConfig fBasedir
  let days = maybe [1 .. length (years ! fYear)] (: []) fDay
  mapM_ (run config fInput fYear) days
