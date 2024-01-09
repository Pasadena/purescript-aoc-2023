module Day16 (day16PartOne, day16PartTwo) where

import Prelude

import Data.Array (concat, foldl, fromFoldable, head, uncons, (..))
import Data.Array as Array
import Data.Foldable (maximum)
import Data.List (filter, length)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Console (logShow)
import Lib (readFile)

data Direction = Left | Right | Up | Down

instance showDirection :: Show Direction where
  show Left = "Left"
  show Right = "Right"
  show Up = "Up"
  show Down = "Down"

instance eqDirection :: Eq Direction where
  eq a b = eq (show a) (show b)

instance ordDirection :: Ord Direction where
  compare a b = compare (show a) (show b)

type Beam = {
  id :: String,
  row :: Int,
  col :: Int,
  direction :: Direction
}

showBeam :: Beam -> String
showBeam beam = "Beam { id: " <> show beam.id <> ", row: " <> show beam.row <> ", col: " <> show beam.col <> ", direction: " <> show beam.direction <> " }"

insertEntry :: Char -> Int -> Int -> Map String Char -> Map String Char
insertEntry c rIdx cIdx map = Map.insert (show rIdx <> "_" <> show cIdx) c map

mapRow :: String -> Int -> Map String Char
mapRow row rIdx = fst $ foldl (\acc c ->
  let
    cIdx = snd acc
    withKey = insertEntry c rIdx cIdx (fst acc)
    nextCol = snd(acc) + 1
  in
    (Tuple withKey nextCol)
  ) (Tuple Map.empty 0) (toCharArray row)

mapRows :: Array String -> Map String Char
mapRows rows = fst $ foldl (\acc row ->
  let
    rIdx = snd acc
    currMap = fst acc
    withRow = Map.union currMap (mapRow row rIdx)
  in
    Tuple withRow (rIdx + 1)
  ) (Tuple Map.empty 0) rows

createId :: Int -> Int -> Direction -> String
createId row col dir = show row <> "_" <> show col <> "_" <> show dir

continueLeftwards :: Beam -> Beam
continueLeftwards beam = beam { col = beam.col - 1, direction = Left }

splitBeamLeftwards :: Beam -> Beam
splitBeamLeftwards beam = beam { id = createId beam.row beam.col Left, col = beam.col - 1, direction = Left }

continueRightwards :: Beam -> Beam
continueRightwards beam = beam { col = beam.col + 1, direction = Right  }

splitBeamRightwards :: Beam -> Beam
splitBeamRightwards beam = beam { id = createId beam.row beam.col Right, col = beam.col + 1, direction = Right }

continueDownwards :: Beam -> Beam
continueDownwards beam = beam { row = beam.row + 1, direction = Down }

splitBeamDownwards :: Beam -> Beam
splitBeamDownwards beam = beam { id = createId beam.row beam.col Down, row = beam.row + 1, direction = Down }

continueUpwards :: Beam -> Beam
continueUpwards beam = beam { row = beam.row -1, direction = Up }

splitBeamUpwards :: Beam -> Beam
splitBeamUpwards beam = beam { id = createId beam.row beam.col Up, row = beam.row - 1, direction = Up }

gatherSplinters :: Beam -> Beam -> Set Beam -> Set String -> Set Beam
gatherSplinters first second queue handled = do
  let
    withUpdated = if (Set.member first.id handled) then queue else Set.insert first queue
    withNew = if (Set.member second.id handled) then withUpdated else Set.insert second withUpdated
  withNew
  

trackBeam :: Beam -> Map String Char -> Map String Char -> Set Beam -> Set String -> Tuple (Map String Char) (Set Beam)
trackBeam beam area energized otherBeams handled = do
  let
    currValue = (Map.lookup (show (beam.row) <> "_" <> show (beam.col)) area)
  case currValue of
    Nothing -> Tuple energized otherBeams
    Just x -> do
      let
        updatedArea = Map.insert (show (beam.row) <> "_" <> show (beam.col)) '#' energized
      case x of 
        '/' -> case beam.direction of
            Left -> trackBeam (continueDownwards beam) area updatedArea otherBeams handled
            Right -> trackBeam (continueUpwards beam) area updatedArea otherBeams handled
            Up -> trackBeam (continueRightwards beam) area updatedArea otherBeams handled
            Down -> trackBeam (continueLeftwards beam) area updatedArea otherBeams handled
        '\\' -> case beam.direction of
            Left -> trackBeam (continueUpwards beam) area updatedArea otherBeams handled
            Right -> trackBeam (continueDownwards beam) area updatedArea otherBeams handled
            Up -> trackBeam (continueLeftwards beam) area updatedArea otherBeams handled
            Down -> trackBeam (continueRightwards beam) area updatedArea otherBeams handled
        '|' -> case beam.direction of
            Up -> trackBeam (continueUpwards beam) area updatedArea otherBeams handled
            Down -> trackBeam (continueDownwards beam) area updatedArea otherBeams handled
            Left -> do
              let
                toDown = splitBeamDownwards beam
                toUp = splitBeamUpwards beam
                withSplinters = gatherSplinters toDown toUp otherBeams handled
              Tuple updatedArea withSplinters
            Right -> do
              let
                toDown = splitBeamDownwards beam
                toUp = splitBeamUpwards beam
                withSplinters = gatherSplinters toDown toUp otherBeams handled
              Tuple updatedArea withSplinters
        '-' -> case beam.direction of 
            Left -> trackBeam (continueLeftwards beam) area updatedArea otherBeams handled
            Right -> trackBeam (continueRightwards beam) area updatedArea otherBeams handled
            Up -> do
              let
                toRight = splitBeamRightwards beam
                toLeft = splitBeamLeftwards beam
                withSplinters = gatherSplinters toRight toLeft otherBeams handled
              Tuple updatedArea withSplinters
            Down -> do
              let
                toRight = splitBeamRightwards beam
                toLeft = splitBeamLeftwards beam
                withSplinters = gatherSplinters toRight toLeft otherBeams handled
              Tuple updatedArea withSplinters
        _ -> case beam.direction of
            Left -> trackBeam (continueLeftwards beam) area updatedArea otherBeams handled
            Right -> trackBeam (continueRightwards beam) area updatedArea otherBeams handled
            Up -> trackBeam (continueUpwards beam) area updatedArea otherBeams handled
            Down -> trackBeam (continueDownwards beam) area updatedArea otherBeams handled

trackBeams :: Set Beam -> Map String Char -> Map String Char -> Set String -> Map String Char
trackBeams beams area energized handled =
  case uncons (fromFoldable beams) of
    Nothing -> energized
    Just {head: x, tail: rest } -> do
      let
        res = trackBeam x area energized (Set.fromFoldable rest) handled
      trackBeams (snd res) area (fst res)  (Set.insert x.id handled)

day16PartOne :: Effect Unit
day16PartOne = do
  input <- readFile "src/inputs/day16input.txt"
  let 
    rows = lines input
    rowMap = mapRows rows
    updatedMap = trackBeams (Set.singleton { id: "0_0_Right", row: 0, col:  0, direction: Right }) rowMap Map.empty Set.empty
    mapValues = Map.values updatedMap 
    energized = filter (\x -> x == '#') mapValues
  logShow (length energized)

day16PartTwo :: Effect Unit
day16PartTwo = do
  input <- readFile "src/inputs/day16input.txt"
  let
    rows = lines input
    rowMap = mapRows rows
    maxCol = (String.length $ (fromMaybe ("") (head rows))) - 1
    maxRow = ((Array.length rows) - 2) -- rows contains one empty row
    beams = concat [
      (0 .. maxCol) # map (\c -> { id: createId 0 c Down, row: 0, col: c, direction: Down }),
      (0 .. maxCol) # map (\c -> { id: createId maxRow c Up, row: maxRow, col: c, direction: Up }),
      (0 .. maxRow) # map (\r -> { id: createId r 0 Right, row: r, col: 0, direction: Right }),
      (0 .. maxRow) # map (\r -> { id: createId r maxCol Left, row: r, col: maxCol, direction: Left })
    ]
    
    values = beams # map (\beam ->
      let
        beamRes = trackBeams (Set.singleton beam) rowMap Map.empty Set.empty
        energized = filter (\x -> x == '#') (Map.values beamRes)
      in
        length energized
      )
  logShow (values)
  logShow (maximum values)