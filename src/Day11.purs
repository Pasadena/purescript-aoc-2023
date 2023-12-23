module Day11
  (day11both
  )
  where

import Prelude

import Data.Array (replicate)
import Data.Foldable (sum)
import Data.Int (fromNumber, toNumber)
import Data.List (List(..), (..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.String (Pattern(..), contains, length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

type Galaxy = {
  id :: String,
  r :: Int,
  c :: Int
}
withoutGalaxies :: String -> Boolean
withoutGalaxies row = not (contains (Pattern "#") (row))

countSteps :: Tuple Galaxy Galaxy -> Int
countSteps pair = do
  let
    first = fst pair
    second = snd pair
    xDiff = first.r - second.r
    yDiff = first.c - second.c
    xAbs = abs (toNumber xDiff)
    yAbs = abs (toNumber yDiff)
  fromMaybe 0 (fromNumber xAbs) + fromMaybe 0 (fromNumber yAbs)

formPairs :: List Galaxy -> List (List (Tuple Galaxy Galaxy))
formPairs galaxies = case List.uncons galaxies of
  Just {head: galaxy, tail: rest} -> do
    let
      pairs = rest # map (\g -> Tuple galaxy g)
    pairs : formPairs rest
  Nothing -> Nil

parseGalaxiesInRow :: List Char -> Int -> Int -> List Galaxy
parseGalaxiesInRow row rowNum colNum  = case (List.uncons row) of
  Just { head: x, tail: rest } -> case x of
    'x' -> parseGalaxiesInRow rest (rowNum) (colNum + expandMultiplier)
    '#' -> do
      let
        galaxy = { id: "Row: " <> show rowNum <> ", Column: " <> show colNum, r: rowNum, c: colNum}
      galaxy : parseGalaxiesInRow rest (rowNum) (colNum + 1)
    _ -> parseGalaxiesInRow rest (rowNum) (colNum + 1)
  Nothing -> Nil

parseGalaxies :: List (List Char) -> Int -> List (List Galaxy)
parseGalaxies rows rowNum = case List.uncons rows of
  Just { head: row, tail: rest } -> case List.head row of
    Just c -> case c of
      'x' -> parseGalaxies rest (rowNum + expandMultiplier)
      _ -> do
        let
          galaxies = parseGalaxiesInRow row rowNum 0
        galaxies : parseGalaxies rest (rowNum + 1)
    Nothing -> Nil
  Nothing -> Nil


expandRows :: List String -> List (List Char)
expandRows universe = do
  let
    withExpandedRows :: List String
    withExpandedRows = List.foldl (\acc row ->
    ( let
        withRow = (List.snoc acc row)
        markers = replicate (length row) 'x'
        expandMarker = fromCharArray markers
      in
        if (withoutGalaxies row)
          then List.snoc withRow expandMarker
          else withRow
        )) Nil universe
    asChars = withExpandedRows # map (\row -> List.fromFoldable (toCharArray row))
    columnsAsRows = List.transpose (asChars)
    withExpandedCols = List.foldl (\acc row ->
    ( let
        withRow = (List.snoc acc row)
        expandMarkers = 0 .. (List.length row) # map (\_ -> 'x')
        galaxyCount = List.length $ List.filter (\y -> y == '#') row
      in
        if (galaxyCount == 0)
          then ((List.snoc withRow expandMarkers))
          else withRow
        )) Nil columnsAsRows
  List.transpose withExpandedCols

expandMultiplier :: Int
expandMultiplier = 99999

-- you can run both parts with this by changing the value of expandMultiplier:
-- value for part 1 is 1 and value for part two is 99999
day11both :: Effect Unit
day11both = do
  input <- readFile "src/inputs/day11input.txt"
  let
    entries = expandRows (List.fromFoldable (lines input))
    galaxies = List.concat (parseGalaxies entries 0)
    pairs = List.concat (formPairs galaxies)
    steps = pairs # map countSteps
  logShow (sum steps) -- 82000210 -- 82000210
