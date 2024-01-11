module Day11
  ( day11both
  )
  where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (maximum, minimum, sum)
import Data.List (List(..), (..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
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

formPairs :: List Galaxy -> List (List (Tuple Galaxy Galaxy))
formPairs galaxies = case List.uncons galaxies of
  Just {head: galaxy, tail: rest} -> do
    let
      pairs = rest # map (\g -> Tuple galaxy g)
    pairs : formPairs rest
  Nothing -> Nil

parseEmptyRows :: List String -> List Int
parseEmptyRows input = do
  let
    empties = List.foldl(\acc row ->
      let
        rows = fst acc
        idx = snd acc
        nextIdx = idx + 1
      in
        if (withoutGalaxies row)
          then Tuple (List.snoc rows idx) (nextIdx)
          else Tuple rows nextIdx
    ) (Tuple Nil 0) input
  fst empties

colVals :: List String -> Int -> String
colVals input col = fromCharArray $ fromFoldable $ input # map (\row -> fromMaybe ('X') (charAt col row))

parseEmptyCols :: List String -> List Int
parseEmptyCols input = do
  let
    emptyCols = 0 .. ((List.length input) -1) # map (\c ->
      let 
        colRow = colVals input c
      in
        if (withoutGalaxies colRow) then Just c  else Nothing
    )
  List.catMaybes emptyCols

  

countPoints :: List (Tuple Galaxy Galaxy) -> List Int -> List Int -> Int
countPoints galaxyPairs emptyRows emptyCols = do
  let
    points :: List Int
    points = List.foldl(\(acc) pair ->
      let
        first = fst pair
        second = snd pair
        minR = fromMaybe (0) (minimum [first.r, second.r])
        maxR = fromMaybe (0) (maximum [first.r, second.r])
        minC = fromMaybe (0) (minimum [first.c,second.c])
        maxC = fromMaybe (0) (maximum [first.c, second.c])
        xDistance = if first.r == second.r then Nil else List.range(minR + 1) (maxR) # map (\r ->
          case List.elemIndex r emptyRows of
          Just _ -> expandMultiplier
          Nothing -> 1
        )
        yDistance = if first.c == second.c then Nil else List.range (minC + 1) (maxC) # map (\r ->
          case List.elemIndex r emptyCols of
          Just _ -> expandMultiplier
          Nothing -> 1
        )
        distance = (sum xDistance) + (sum yDistance)
      in
        distance : acc
    ) Nil galaxyPairs
  sum points


parseGalaxiesInRow :: List Char -> Int -> Int -> List Galaxy
parseGalaxiesInRow row rowNum colNum = case (List.uncons row) of
  Just { head: x, tail: rest } -> case x of
    '#' -> do
      let
        galaxy = { id: "Row: " <> show rowNum <> ", Column: " <> show colNum, r: rowNum, c: colNum}
      galaxy : parseGalaxiesInRow rest (rowNum) (colNum + 1)
    _ -> parseGalaxiesInRow rest (rowNum) (colNum + 1)
  Nothing -> Nil

parseGalaxies :: List String -> Int -> List (List Galaxy)
parseGalaxies rows rowNum = case List.uncons rows of
  Just { head: row, tail: rest } -> do
    let
      galaxies = parseGalaxiesInRow (List.fromFoldable (toCharArray row)) rowNum 0
    galaxies : parseGalaxies rest (rowNum + 1)
  Nothing -> Nil

expandMultiplier :: Int
expandMultiplier = 100000

-- you can run both parts with this by changing the value of expandMultiplier:
-- value for part 1 is 1 and value for part two is 100000
day11both :: Effect Unit
day11both = do
  input <- readFile "src/inputs/day11input.txt"
  let
    rows = List.fromFoldable (lines input)
    galaxies = List.concat (parseGalaxies rows 0)
    pairs = List.concat (formPairs galaxies)
    
    emptyRows = parseEmptyRows rows
    emptyCols = parseEmptyCols rows
    points = countPoints pairs emptyRows emptyCols
  logShow (points)