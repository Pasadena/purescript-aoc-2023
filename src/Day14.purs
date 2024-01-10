module Day14
  ( day14PartOne
  -- , day14PartTwo
  )
  where

import Prelude

import Data.Array (filter, foldl, fromFoldable, length, (!!), (..), (:))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)

type MovableRock = {
    r :: Int,
    c :: Int
}

data Direction = Left | Right | Up | Down

type RockMap = Map String Char

insertEntry :: Char -> Int -> Int -> RockMap -> RockMap
insertEntry c rIdx cIdx map = Map.insert (show rIdx <> "_" <> show cIdx) c map

mapRow :: String -> Int -> RockMap
mapRow row rIdx = fst $ foldl (\acc c ->
  let
    cIdx = snd acc
    withKey = insertEntry c rIdx cIdx (fst acc)
    nextCol = snd(acc) + 1
  in
    (Tuple withKey nextCol)
  ) (Tuple Map.empty 0) (toCharArray row)

mapRows :: Array String -> RockMap
mapRows rows = fst $ foldl (\acc row ->
  let
    rIdx = snd acc
    currMap = fst acc
    withRow = Map.union currMap (mapRow row rIdx)
  in
    Tuple withRow (rIdx + 1)
  ) (Tuple Map.empty 0) rows

rocksFromRow :: String -> Int -> Array MovableRock
rocksFromRow input rowNum = do
  let
    rocks :: Array MovableRock
    rocks = fst $ foldl (\acc c ->
      if c == 'O'
      then
        let
          rock = { r: rowNum, c: (snd acc)}
        in
          Tuple (rock : (fst acc)) (snd acc + 1)
      else
        let 
          nextCol = (snd acc + 1)
        in
            (Tuple (fst acc) nextCol)
        ) (Tuple [] 0) (toCharArray input)
  rocks

nextRockPos :: MovableRock -> Direction -> MovableRock
nextRockPos rock Left = { r: rock.r, c: rock.c - 1 }
nextRockPos rock Right = { r: rock.r, c: rock.c + 1 }
nextRockPos rock Up = { r: rock.r - 1, c: rock.c }
nextRockPos rock Down = { r: rock.r + 1, c: rock.c }

previousRockPos :: MovableRock -> Direction -> MovableRock
previousRockPos rock Left = { r: rock.r, c: rock.c + 1 }
previousRockPos rock Right = { r: rock.r, c: rock.c - 1 }
previousRockPos rock Up = { r: rock.r + 1, c: rock.c }
previousRockPos rock Down = { r: rock.r - 1, c: rock.c }

tiltRock :: MovableRock -> Direction -> RockMap -> MovableRock
tiltRock rock direction area = do
  let
    nextPos = nextRockPos rock direction
    currValue = (Map.lookup (show (nextPos.r) <> "_" <> show (nextPos.c)) area)
  case currValue of 
    Nothing -> rock
    Just val -> case val of
      '.' -> tiltRock nextPos direction area
      _ -> rock

tiltRockToDirections :: MovableRock -> Array Direction -> RockMap -> RockMap
tiltRockToDirections rock directions area = foldl(\acc direction ->
  let
    updatedRock = tiltRock rock direction acc
    withUpdatedRock = Map.alter (\_ -> Just 'O') (show (updatedRock.r) <> "_" <> show (updatedRock.c)) acc
  in
    if (rock.r == updatedRock.r && rock.c == updatedRock.c)
    then
      withUpdatedRock
    else
      Map.alter (\_ -> Just '.') (show (rock.r) <> "_" <> show (rock.c)) withUpdatedRock
) area directions

tiltRocks :: Array String -> Array Direction -> RockMap -> RockMap
tiltRocks input directions area = do
  let
    rows = 0 .. (length input - 1)
    updatedArea = foldl (\currMap rowNum ->
      let
        row = fromMaybe ("") (input !! rowNum)
        rocks = rocksFromRow row rowNum
        tilted = foldl (\accArea rock -> tiltRockToDirections rock directions accArea
        ) currMap rocks
      in
        tilted
    ) area rows
  updatedArea

countLoad :: Array String -> Int -> Int
countLoad rocks offset = sum $ rocks # map (\key -> 
  let
    parts = split (Pattern ("_")) key
    r = case parts !! 0 of
      Just val -> offset - fromMaybe (0) (fromString val)
      Nothing -> 0
  in
    r
)

day14PartOne :: Effect Unit
day14PartOne = do
  input <- readFile "src/inputs/day14input.txt"
  let
    rows = (lines input)
    rockMap = mapRows rows
    tilted = tiltRocks rows [Up] rockMap
    keys = filter (\k -> case (Map.lookup k tilted) of
      Just val -> val == 'O'
      Nothing -> false
    ) $ fromFoldable $ Map.keys rockMap

  logShow (countLoad keys (length rows))


-- tiltMany :: Array String -> Array Direction -> RockMap -> Int -> RockMap
-- tiltMany rows directions area iteration = case iteration of 
--   0 -> area
--   _ -> do
--     let
--       tilted = tiltRocks rows directions area
--       _ = spy("ITERATION DONE") iteration
--     tiltMany rows directions tilted (iteration - 1)

-- day14PartTwo :: Effect Unit
-- day14PartTwo = do
--   input <- readFile "src/inputs/day14input.txt"
--   let
--     directions = [Up, Left, Down, Right]
--     rows = (lines input)
--     rockMap = mapRows rows
--     -- tilted = foldl(\ acc c -> 
--     --   let
--     --     _ = spy("ROUND") c
--     --   in
--     --     tiltRocks rows directions rockMap) rockMap [1..100]
--     tilted = tiltMany rows directions rockMap 1000000000
--     keys = filter (\k -> case (Map.lookup k tilted) of
--       Just val -> val == 'O'
--       Nothing -> false
--     ) $ fromFoldable $ Map.keys rockMap

--   -- logShow tilted
--   logShow (countLoad keys (length rows))