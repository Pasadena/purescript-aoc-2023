module Day9
  ( day9partOne
  )
  where

import Prelude

import Data.Array (all, catMaybes, foldl, last, reverse, uncons, (:))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, words)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

difference :: Array Int -> Array Int
difference history = case uncons history of
  Just { head: x, tail: rest } -> case uncons rest of
    Just { head: y, tail: _ } -> (y - x) : difference rest
    Nothing -> []
  Nothing -> []

extrapolate :: Array Int -> Array (Array Int)
extrapolate history = do
  let nextDiff = difference history
  let allZeros = all (_ == 0) nextDiff
  if allZeros then ([nextDiff]) else (nextDiff : (extrapolate nextDiff))

nextValue :: Array Int -> Int
nextValue history = do
  let
    extrapolatedHistory = history : extrapolate history
    currentValues = reverse $ catMaybes $ map (last) extrapolatedHistory
  foldl (\acc value -> acc + value) 0 currentValues

parseHistoryEntry :: String -> Array Int
parseHistoryEntry line = do
  let
    entries :: Array (Maybe Int)
    entries = map fromString $ words line
  catMaybes entries

day9partOne :: Effect Unit
day9partOne = do
  input <- readFile "src/inputs/day9input.txt"
  let
    entries = map parseHistoryEntry $ lines input
    nextVals = map nextValue entries
  logShow (sum nextVals)