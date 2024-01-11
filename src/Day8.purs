module Day8 (day8partOne, day8partTwo) where

import Prelude

import Data.Array (dropWhile, foldl, fromFoldable, head, last, length, tail, takeWhile, (!!))
import Data.List (List, filter)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, lastIndexOf, replace, split)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

type Instructions = {
    instructions :: String,
    nodes :: List Node
}

type Node = {
    id :: String,
    left :: String,
    right :: String
}
-- TODO: do this with parser
parseLine :: String -> Node
parseLine line = do
  let 
    idAndOthers = split (Pattern (" = ")) line
    id = fromMaybe ("") (head idAndOthers)
    leftAndRight = split (Pattern (", "))  (fromMaybe ("") (last idAndOthers))
    left = fromMaybe ("") (head leftAndRight) 
    right = fromMaybe ("") (last leftAndRight) 
  {
    id: id,
    left: replace (Pattern ("(")) (Replacement ("")) left,
    right: replace (Pattern (")")) (Replacement ("")) right
  }

mapNodes :: Array Node -> Map String Node
mapNodes nodes = foldl(\acc node -> Map.insert (node.id) (node) acc) Map.empty nodes
runInstructions :: Array Char -> String -> Map String Node -> Int -> Int
runInstructions instructions nodeId nodeMap count = do
  let
    nextDir = instructions !! (count `mod` length instructions)
  case nodeId of
    "ZZZ" -> count
    _ -> do
        let
          maybeNode = Map.lookup nodeId nodeMap
        case nextDir of
          Nothing -> count
          Just dir -> case dir of
            'L' -> case maybeNode of
                Nothing -> 0
                Just node -> runInstructions instructions node.left nodeMap (count + 1)
            'R' -> case maybeNode of
                Nothing -> 0
                Just node -> runInstructions instructions node.right nodeMap (count + 1)
            _ -> count      

day8partOne :: Effect Unit
day8partOne = do
  input <- readFile "src/inputs/day8input.txt"
  let
    rows = lines input
    rawInstructions = takeWhile (\l -> l /= "") rows
    instructions = toCharArray $ joinWith ("") rawInstructions
    nodeLines = dropWhile (\l -> l /= "") rows
    nodes = (fromMaybe [] (tail nodeLines)) # map parseLine
    nodeMap = mapNodes nodes

    res = runInstructions instructions "AAA" nodeMap 0
  logShow (res)

endsWithZ :: Array Char -> String -> Map String Node -> Int -> Int
endsWithZ instructions nodeId nodeMap count = do
  let
    nextDir = instructions !! (count `mod` length instructions)
  case String.stripSuffix (Pattern ("Z")) nodeId of
    Just _ -> count
    Nothing -> do
        let
          maybeNode = Map.lookup nodeId nodeMap
        case nextDir of
          Nothing -> count
          Just dir -> case dir of
            'L' -> case maybeNode of
                Nothing -> 0
                Just node -> endsWithZ instructions node.left nodeMap (count + 1)
            'R' -> case maybeNode of
                Nothing -> 0
                Just node -> endsWithZ instructions node.right nodeMap (count + 1)
            _ -> count    

day8partTwo :: Effect Unit
day8partTwo = do
  input <- readFile "src/inputs/day8input.txt"
  let
    rows = lines input
    rawInstructions = takeWhile (\l -> l /= "") rows
    instructions = toCharArray $ joinWith ("") rawInstructions
    nodeLines = dropWhile (\l -> l /= "") rows
    nodes = (fromMaybe [] (tail nodeLines)) # map parseLine
    nodeMap = mapNodes nodes

    initial = fromFoldable $ Map.values nodeMap # filter (\n -> (fromMaybe(-1) (lastIndexOf (Pattern ("A")) n.id)) == (String.length n.id - 1)) # map (_.id)

    res = initial # map (\i -> endsWithZ instructions i nodeMap 0)
    lcmRes = foldl (lcm) 1 res
  logShow (res)
  logShow (lcmRes)