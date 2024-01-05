module Day13
  ( day13PartOne
  , day13PartTwo
  )
  where

import Prelude

import Data.Array (all, catMaybes, concat, drop, filter, head, init, length, tail, take, takeWhile, zip, (!!), (..), (:))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.Utils (charAt, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Lib (readFile)

type Reflection = {
  vertical :: Boolean,
  amount :: Int
}

formGroups :: Array String -> Array (Array String)
formGroups input = do
  if (length input) == 0
    then []
    else
      let
        group = takeWhile (\x -> x /= "") input
        dropAmount = length group + 1
        rest = drop dropAmount input
      in
      group : formGroups rest

parseInput :: String -> Array (Array String)
parseInput input = do
  let 
    patterns = split (Pattern "\n") input
  formGroups patterns

-- TODO: sweet Hesus this looks bad --> refactor
hasReflectingLine :: (Array String) -> Int -> Maybe Reflection
hasReflectingLine group allowedDiff = do
  let
    height = length group
    rows :: Array Int
    rows = 0 .. (height - 1)
    reflections = filter (\c -> (
      let
        nextIdx = c + 1
        line = fromMaybe ("") (group !! c)
        next = fromMaybe ("") (group !! nextIdx)
      in
        if allowedDiff == 0 && line == next
          then
            let
              fromStart = take c group
              toEnd = drop (c + 2) group
              zipped = pairStrings (fromStart) (toEnd)
            in
              zipped # all (\ (Tuple a b) -> a == b)
          else if allowedDiff == 0 && line /= next
          then false
          else
            let
              zipped = zip (toCharArray line) (toCharArray next)
              diffsFromLine = map (\ (Tuple a b) -> if a == b then 0 else 1) zipped
              fromStart = take c group
              toEnd = drop (c + 2) group
              neighbors = pairStrings (fromStart) (toEnd)
              diffNeighbors = concat $ map (\ (Tuple a b) ->
                let
                  n1 = toCharArray a
                  n2 = toCharArray b
                  chars = zip n1 n2
                in
                  chars # map (\ (Tuple c1 c2) -> if c1 == c2 then 0 else 1)
              ) neighbors
            in
              (sum diffsFromLine) + (sum diffNeighbors) == 1
    )) rows
  case head reflections of
    Just x -> Just { vertical: false, amount: (x + 1) }
    Nothing -> Nothing

-- TODO: generic pairing function
pairStrings :: Array String -> Array String -> Array (Tuple String String)
pairStrings fromStart toEnd = if (length fromStart) == 0 || (length toEnd) == 0
  then []
  else
    let
      first = fromMaybe ("") (fromStart !! ((length fromStart) - 1))
      last = fromMaybe ("") (head toEnd)
      start = fromMaybe ([]) (init fromStart)
      end = fromMaybe ([]) (tail toEnd)
    in
      Tuple first last : pairStrings start end

pair :: Array Int -> Array Int -> Array (Tuple Int Int)
pair fromStart toEnd = if (length fromStart) == 0 || (length toEnd) == 0
  then []
  else
    let
      first = fromMaybe (0) (fromStart !! ((length fromStart) - 1))
      last = fromMaybe (0) (head toEnd)
      start = fromMaybe ([]) (init fromStart)
      end = fromMaybe ([]) (tail toEnd)
    in
      Tuple first last : pair start end

-- TODO: sweet Hesus this also looks bad --> refactor  
columnReflects :: (Array String) -> Int -> Maybe Reflection
columnReflects group diffAmount = do
  let
    first = fromMaybe ("") (head group)
    width :: Int
    width = String.length $ first
    cols :: Array Int
    cols = 0 .. (width - 1)
    perfectReflections = filter (\c -> (
      let
        nextIdx = c + 1
        elems :: Array String
        elems = catMaybes $ group # map (\x -> charAt c x)
        next = group # map (\x -> (
          let
            nextChar = (charAt nextIdx x)
          in
            fromMaybe ("") nextChar
        ))
      in
        if diffAmount == 0 && elems == next
          then
            let
              fromStart = take c cols
              toEnd = drop (c + 2) cols
              zipped = pair fromStart toEnd
            in
              zipped # all (\ (Tuple a b) -> (
              let
                n1 = catMaybes $ group # map (\y -> charAt a y)
                n2 = group # map (\y -> (
                let
                  nextChar = (charAt b y)
                in
                  fromMaybe ("") nextChar
              ))
              in
                n1 == n2
            ))
          else if diffAmount == 0 && elems /= next
            then false
          else
            let
              zipped = zip (elems) (next)
              diffsFromLine = map (\ (Tuple a b) -> if a == b then 0 else 1) zipped
              fromStart = take c cols
              toEnd = drop (c + 2) cols
              neighbors = pair (fromStart) (toEnd)
              diffNeighbors = concat $ map (\ (Tuple a b) ->
                let
                  n1 = catMaybes $ group # map (\x -> charAt a x)
                  n2 = group # map (\x -> (
                    let
                      firstChar = fromMaybe ("") (charAt 0 x)
                      nextChar = (charAt b x)
                    in
                      fromMaybe (firstChar) nextChar
                  ))
                  chars = zip n1 n2
                in
                  chars # map (\ (Tuple c1 c2) -> if c1 == c2 then 0 else 1)
              ) neighbors
            in
              (sum diffsFromLine) + (sum diffNeighbors) == 1
    )) cols
  case head perfectReflections of
    Just x -> Just { vertical: true, amount: (x + 1) }
    Nothing -> Nothing

findReflections :: (Array String) -> Int -> Maybe Reflection
findReflections group diffAmount = do
  let
    vertical = columnReflects group diffAmount
    horizontal = hasReflectingLine group diffAmount
  case horizontal, vertical of
    Just x, Nothing  -> pure x
    Nothing, Just y -> pure y
    Nothing, Nothing -> Nothing
    _, _ -> Nothing

day13PartOne :: Effect Unit
day13PartOne = do
  input <- readFile "src/inputs/day13input.txt"
  let 
    patterns = parseInput input
    matches = patterns # map (\g -> findReflections g 0)
    reflections = catMaybes matches
    verticals :: Int
    verticals = sum $ filter (\x -> x.vertical == true) reflections # map (\x -> x.amount)
    hReflections = filter (\x -> x.vertical == false) reflections
    horizontals :: Int
    horizontals = sum $ hReflections # map (\x -> x.amount * 100)
  logShow (length patterns)
  logShow (length reflections)
  logShow (matches)
  logShow (verticals + horizontals)

day13PartTwo :: Effect Unit
day13PartTwo = do
  input <- readFile "src/inputs/day13input.txt"
  let 
    patterns = parseInput input
    matches = patterns # map (\g -> findReflections g 1)
    reflections = catMaybes matches
    verticals :: Int
    verticals = sum $ filter (\x -> x.vertical == true) reflections # map (\x -> x.amount)
    hReflections = filter (\x -> x.vertical == false) reflections
    horizontals :: Int
    horizontals = sum $ hReflections # map (\x -> x.amount * 100)
  logShow (length patterns)
  logShow (length reflections)
  logShow (matches)
  logShow (verticals + horizontals)