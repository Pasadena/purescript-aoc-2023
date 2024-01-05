module Day4
  ( day4partOne
  , day4partTwo
  )
  where

import Prelude

import Data.Array (foldl, (..))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (pow)
import Data.List (List(..), intersect, length,  uncons)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (readFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, manyTill, sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal, space)

type Card = {
  id :: Int,
  matches :: Int
}
parseNumberWithSpaces :: Parser String Int
parseNumberWithSpaces = do
  number <- intDecimal
  _ <- many (char ' ')
  pure number

parseCard :: Parser String Card
parseCard = do
  _ <- string "Card"
  _ <- many space
  id <- intDecimal
  _ <- string ":"
  _ <- many space
  winningNumbers <- manyTill parseNumberWithSpaces (string "|")
  _ <- many space
  hand <- many parseNumberWithSpaces
  pure {
    id: id,
    matches: length $ (intersect winningNumbers hand)
  }

pointsFromCard :: Card -> Int
pointsFromCard card = do
  case card.matches of
    0 -> 0
    1 -> 1
    2 -> 2
    x -> pow 2 (x - 1)

day4partOne :: Effect Unit
day4partOne = do
  input <- readFile "src/inputs/day4input.txt"
  let
    cards = case runParser input (parseCard `sepBy` char '\n') of
      Right x -> x
      Left _ -> Nil
    pointSum = cards # map pointsFromCard # sum
  logShow pointSum

-------------- PART 2 ----------------

countCopies :: List Card -> Map Int Int -> Map Int Int 
countCopies cards copyMap = case uncons cards of
  Just { head: x, tail: xs} -> case x.matches of
    0 -> countCopies xs copyMap
    amount -> do
      let
        id = x.id
        ids = (id + 1) .. (id + amount)
        copies = fromMaybe (1) (Map.lookup id copyMap)
        updated = foldl (\acc arrId -> 
          Map.alter (\value -> case value of
              Just val -> Just (val + copies)
              Nothing -> Just 1
              ) arrId acc
          ) copyMap ids
      countCopies xs updated
  Nothing -> copyMap

day4partTwo :: Effect Unit
day4partTwo = do
  input <- readFile "src/inputs/day4input.txt"
  let
    cards = case runParser input (parseCard `sepBy` char '\n') of
      Right x -> x
      Left _ -> Nil
    tuples = cards # map (\card -> (Tuple card.id 1))
    initialMap = Map.fromFoldable tuples
    withCopies = countCopies (cards) (initialMap)
  logShow (sum $ Map.values withCopies)