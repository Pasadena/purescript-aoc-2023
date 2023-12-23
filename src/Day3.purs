module Day3
  ( day3partOne
  , day3partTwo
  )
  where

import Prelude

import Data.Array (concat, elem, filter, foldl, fromFoldable, intersect, range, snoc, uncons, zip, (..), (:))
import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (codePointFromChar, countPrefix, drop, length, take)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

type PartSymbol = {
  r :: Int,
  c :: Int,
  value :: Char
}

type PartNumber = {
  r :: Int,
  c :: Int,
  value :: Int
}

isSymbol :: Char -> Boolean
isSymbol x = x /= '.' && not (isDecDigit (codePointFromChar x))

parseSymbolsInRow :: String -> Int -> Array PartSymbol
parseSymbolsInRow input rowNum = do
  let
    chars = toCharArray input
    withIndexes :: Array (Tuple Char Int)
    withIndexes = zip (chars) (0 .. (Array.length chars))
  foldl (\acc x -> (
    let
      c = (fst x)
      col = (snd x)
    in
      if (isSymbol c)
        then (snoc acc { r: rowNum, c: col, value: c })
        else acc
  )) [] withIndexes

parseNumbersInRow :: String -> Int -> Int -> Array PartNumber
parseNumbersInRow input rowNum colNum = do
    let
      numberCount = countPrefix (isDecDigit) input
      numbers = fromMaybe 0 (fromString (take numberCount input))
    if length input == 0
      then []
      else if numberCount == 0
        then parseNumbersInRow (drop 1 input) rowNum (colNum + 1)
        else {r: rowNum, c: colNum, value: numbers} : parseNumbersInRow (drop numberCount input) rowNum (colNum + numberCount)


parseNumbers :: Array String -> Int -> Array (Array PartNumber)
parseNumbers rows curRow = case uncons rows of
  Just { head: x, tail: rest} -> (parseNumbersInRow x curRow 0) : (parseNumbers rest (curRow + 1))
  Nothing -> []

parseSymbols :: Array String -> Int -> Array (Array PartSymbol)
parseSymbols rows curRow = case uncons rows of
  Just { head: x, tail: rest} -> (parseSymbolsInRow x curRow) : (parseSymbols rest (curRow + 1))
  Nothing -> []

filterOrphans :: Array PartSymbol -> Array PartNumber -> Array PartNumber
filterOrphans symbols numbers = foldl (\acc x -> (
  let
    valueLength = length (show x.value)
    adjacentCols = range (x.c - 1) (x.c + valueLength)
    adjacentRows = [x.r - 1, x.r, x.r + 1]
    adjacentSymbols = filter (\sym -> (elem sym.c adjacentCols) && (elem sym.r adjacentRows)) symbols
  in
    if (Array.length adjacentSymbols > 0)
      then (snoc acc x)
      else acc
)) [] numbers

day3partOne :: Effect Unit
day3partOne = do
  input <- readFile "src/inputs/day3input.txt"
  let
    inputArray = fromFoldable (lines input)
    symbols = concat $ parseSymbols inputArray 0
    numbers = concat $ parseNumbers inputArray 0
    withoutOrphans = filterOrphans symbols numbers
  logShow (sum (withoutOrphans # map (_.value)))

---------------- PART TWO ----------------

findGears :: Array PartSymbol -> Array PartNumber -> Array (Tuple PartNumber PartNumber)
findGears symbols numbers = foldl (\acc x -> (
  let
    adjacentCols = range (x.c - 1) (x.c + 1)
    adjacentRows = [x.r - 1, x.r, x.r + 1]
    adjacentNumbers = filter (\num -> (
      let
        valueLength = length (show num.value)
        numberCols = range (num.c) (num.c + valueLength -1)
        intersection = intersect adjacentCols numberCols
      in
        (Array.length intersection) > 0 && (elem num.r adjacentRows)
      )
    ) numbers
  in
    if (Array.length adjacentNumbers == 2)
      then (
        let
          first :: PartNumber
          first = fromMaybe { c: 0, r: 0, value: 0} (Array.index adjacentNumbers 0)
          second :: PartNumber
          second = fromMaybe { c: 0, r: 0, value: 0}  (Array.index adjacentNumbers 1)
        in
          snoc acc (Tuple first second)
      )
      else acc
)) [] symbols
day3partTwo :: Effect Unit
day3partTwo = do
  input <- readFile "src/inputs/day3input.txt"
  let
    inputArray = fromFoldable (lines input)
    symbols = concat $ parseSymbols inputArray 0
    numbers = concat $ parseNumbers inputArray 0
    gearSymbols = filter (\x -> x.value == '*') symbols
    gears = findGears gearSymbols numbers
  logShow (sum (gears # map (\x -> (fst x).value * (snd x).value)))