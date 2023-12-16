module Day6
  ( day6partOne
  , day6partTwo
  , parseDistances
  )
  where

import Prelude

import Data.Array (head, last, uncons)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), many, zip)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String.Utils (lines)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (listToArray, readFile)
import Parsing (Parser, runParser)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, space)

type Race = {
  time :: Number,
  distance :: Number
}

parseNumber :: Parser String Number
parseNumber = do
  _ <- many space
  number <- intDecimal
  pure (toNumber number)
parseTimes :: Parser String (List (Number))
parseTimes = do
  _ <- string "Time:"
  times <- many parseNumber
  pure times

parseDistances :: Parser String (List (Number))
parseDistances = do
  _ <- string "Distance:"
  distances <- many parseNumber
  pure distances

combinationsFromRace :: Race -> Number -> Number -> Number
combinationsFromRace race secondsHeld combinations = case (secondsHeld == race.time -1.0) of
  true -> combinations
  false -> do
    let distance = secondsHeld * (race.time - secondsHeld)
    let nextSecond = (secondsHeld + 1.0) 
    if distance > race.distance
      then combinationsFromRace race nextSecond (combinations + 1.0)
      else combinationsFromRace race nextSecond combinations
    

calculateRaces :: Array Race -> Number -> Number
calculateRaces races total = case uncons races of
  Just { head: x, tail: xs} -> do
    let totalFromRace = combinationsFromRace x 1.0 0.0
    let newTotal = total * totalFromRace
    calculateRaces xs newTotal
  Nothing -> total

day6partOne :: Effect Unit
day6partOne = do
  input <- readFile "src/inputs/day6input.txt"
  let
    inputLines = lines input
    timesInput = case head inputLines of
      Just x -> x
      Nothing -> ""
    distancesInput = case last inputLines of
      Just x -> x
      Nothing -> ""
    times = case (runParser timesInput parseTimes) of
      Right x -> x
      Left _ -> Nil
    distances = case (runParser distancesInput parseDistances) of
      Right x -> x
      Left _ -> Nil
    races = zip (times) (distances) # map \tuple -> { time: (fst tuple), distance: (snd tuple)}
  logShow (calculateRaces (listToArray races) 1.0)

-------------------- Part 2 ----------------------
joinList :: Array String -> String
joinList list = case uncons list of
  Just { head: x, tail: xs} -> do
    let res = x <> (joinList xs)
    res
  Nothing -> ""

parseNumberString :: Parser String String
parseNumberString = do
  _ <- many space
  number <- intDecimal
  pure (toString (toNumber number))

parseTimesAsStrings :: Parser String String
parseTimesAsStrings = do
  _ <- string "Time:"
  times <- many parseNumberString
  pure (joinList (listToArray times))

parseDistancesAsStrings :: Parser String String
parseDistancesAsStrings = do
  _ <- string "Distance:"
  distances <- many parseNumberString
  pure (joinList (listToArray distances))

day6partTwo :: Effect Unit
day6partTwo = do
  input <- readFile "src/inputs/day6input.txt"
  let
    inputLines = lines input
    timesInput = case head inputLines of
      Just x -> x
      Nothing -> ""
    distancesInput = case last inputLines of
      Just x -> x
      Nothing -> ""
    timeString = case (runParser timesInput parseTimesAsStrings) of
      Right x -> x
      Left _ -> ""
    distanceString = case (runParser distancesInput parseDistancesAsStrings) of
      Right x -> x
      Left _ -> ""
    time = case fromString timeString of
      Just x -> x
      Nothing -> 0.0
    distance = case fromString distanceString of
      Just x -> x
      Nothing -> 0.0
    race = { time: time, distance: distance }
    combinations = combinationsFromRace race 0.0 0.0
  logShow combinations