module Day1 where

import Prelude

import Data.Array (reverse)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (maximum, minimum, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), drop, dropWhile, indexOf, lastIndexOf, replace, take)
import Data.String.CodeUnits (fromCharArray, toCharArray, uncons)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

reverseString :: String -> String
reverseString = fromCharArray <<< reverse <<< toCharArray

dropUntilFirstDigit :: String -> String
dropUntilFirstDigit input = dropWhile (\c -> (not (isDecDigit c))) input

parseNumber :: String -> Int
parseNumber input = do
  let untilFirstDec = dropUntilFirstDigit input
  case uncons untilFirstDec of
    Just {head: x, tail: rest } -> case uncons (dropUntilFirstDigit (reverseString rest)) of
      Just {head: y, tail: _ } -> fromMaybe (99999999) (fromString (fromCharArray [x, y]))
      Nothing -> fromMaybe (99999999) (fromString (fromCharArray [x, x]))
    Nothing -> 0

firstIndexOfDigit :: String -> Int
firstIndexOfDigit input = do
  let oneIdx = fromMaybe (99999999) (indexOf (Pattern "1") input)
  let twoIdx = fromMaybe (99999999) (indexOf (Pattern "2") input)
  let threeIdx = fromMaybe (99999999) (indexOf (Pattern "3") input)
  let fourIdx = fromMaybe (99999999) (indexOf (Pattern "4") input)
  let fiveIdx = fromMaybe (99999999) (indexOf (Pattern "5") input)
  let sixeIdx = fromMaybe (99999999) (indexOf (Pattern "6") input)
  let sevenIdx = fromMaybe (99999999) (indexOf (Pattern "7") input)
  let eightIdx = fromMaybe (99999999) (indexOf (Pattern "8") input)
  let nineIdx = fromMaybe (99999999) (indexOf (Pattern "9") input)
  let min = minimum [oneIdx, twoIdx, threeIdx, fourIdx, fiveIdx, sixeIdx, sevenIdx, eightIdx, nineIdx]
  fromMaybe (999999) min

replaceFirstWord :: String -> String
replaceFirstWord input = do
  let firstIndexOfNumber = firstIndexOfDigit input
  let oneIdx = fromMaybe (99999999)  (indexOf (Pattern "one") input)
  let twoIdx = fromMaybe (99999999)  (indexOf (Pattern "two") input)
  let threeIdx = fromMaybe (99999999)  (indexOf (Pattern "three") input)
  let fourIdx = fromMaybe (99999999)  (indexOf (Pattern "four") input)
  let fiveIdx = fromMaybe (99999999)  (indexOf (Pattern "five") input)
  let sixeIdx = fromMaybe (99999999)  (indexOf (Pattern "six") input)
  let sevenIdx = fromMaybe (99999999)  (indexOf (Pattern "seven") input)
  let eightIdx = fromMaybe (99999999)  (indexOf (Pattern "eight") input)
  let nineIdx = fromMaybe (99999999)  (indexOf (Pattern "nine") input)
  let min = fromMaybe 9999999 (minimum [firstIndexOfNumber, oneIdx, twoIdx, threeIdx, fourIdx, fiveIdx, sixeIdx, sevenIdx, eightIdx, nineIdx])
  case min of
    n
      | n == (999999) -> input
      | n == oneIdx -> replace (Pattern "one") (Replacement "1") input
      | n == twoIdx -> replace (Pattern "two") (Replacement "2") input
      | n == threeIdx -> replace (Pattern "three") (Replacement "3") input
      | n == fourIdx -> replace (Pattern "four") (Replacement "4") input
      | n == fiveIdx -> replace (Pattern "five") (Replacement "5") input
      | n == sixeIdx -> replace (Pattern "six") (Replacement "6") input
      | n == sevenIdx -> replace (Pattern "seven") (Replacement "7") input
      | n == eightIdx -> replace (Pattern "eight") (Replacement "8") input
      | n == nineIdx -> replace (Pattern "nine") (Replacement "9") input
    _ -> input

replaceLastWord :: String -> String
replaceLastWord input = do
  let oneIdx = fromMaybe (-1) (lastIndexOf (Pattern "one") input)
  let twoIdx = fromMaybe (-1) (lastIndexOf (Pattern "two") input)
  let threeIdx = fromMaybe (-1) (lastIndexOf (Pattern "three") input)
  let fourIdx = fromMaybe (-1) (lastIndexOf (Pattern "four") input)
  let fiveIdx = fromMaybe (-1) (lastIndexOf (Pattern "five") input)
  let sixeIdx = fromMaybe (-1) (lastIndexOf (Pattern "six") input)
  let sevenIdx = fromMaybe (-1) (lastIndexOf (Pattern "seven") input)
  let eightIdx = fromMaybe (-1) (lastIndexOf (Pattern "eight") input)
  let nineIdx = fromMaybe (-1) (lastIndexOf (Pattern "nine") input)
  let max = fromMaybe 9999999 (maximum [oneIdx, twoIdx, threeIdx, fourIdx, fiveIdx, sixeIdx, sevenIdx, eightIdx, nineIdx])
  case max of
    n
      | n == (-1) -> input
      | n == oneIdx -> (take (oneIdx) input) <> "1" <> (drop (oneIdx + 3) input)
      | n == twoIdx -> (take (twoIdx) input) <> "2" <> (drop (twoIdx + 3) input)
      | n == threeIdx -> (take (threeIdx) input) <> "3" <> (drop (threeIdx + 5) input)
      | n == fourIdx -> (take (fourIdx) input) <> "4" <> (drop (fourIdx + 4) input)
      | n == fiveIdx -> (take (fiveIdx) input) <> "5" <> (drop (fiveIdx + 4) input)
      | n == sixeIdx -> (take (sixeIdx) input) <> "6" <> (drop (sixeIdx + 3) input)
      | n == sevenIdx -> (take (sevenIdx) input) <> "7" <> (drop (sevenIdx + 5) input)
      | n == eightIdx -> (take (eightIdx) input) <> "8" <> (drop (eightIdx + 5) input)
      | n == nineIdx -> (take (nineIdx) input) <> "9" <> (drop (nineIdx + 4) input)
    _ -> input

day1partOne :: Effect Unit
day1partOne = do
  input <- readFile "src/inputs/day1input.txt"
  let lineNumbers = lines input # map (\line -> parseNumber line )
  logShow (sum lineNumbers)
  
day1partTwo :: Effect Unit
day1partTwo = do
  input <- readFile "src/inputs/day1input.txt"
  let lineNumbersPart2 = lines input # map replaceFirstWord # map (replaceLastWord >>> parseNumber)
  logShow (sum lineNumbersPart2)