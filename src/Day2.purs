module Day2
  (partOne
  , partTwo
  )
  where

import Prelude

import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.List (List(..), filter, length)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (intFromMaybe, listToArray, readFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, optional, (<|>))
import Parsing.String (string, char)
import Parsing.String.Basic (intDecimal, whiteSpace)

type Cube = {
  color :: String,
  amount :: Int
}

type GameWithValidity = {
  id :: Int,
  valid :: Boolean
}

type GameWihtMinAmounts = {
  id :: Int,
  minAmount :: Int
}

parseMove :: Parser String Boolean
parseMove = do
  amount <- intDecimal
  _ <- whiteSpace
  color <- string "green" <|> string "red" <|> string "blue"
  _ <- optional (string ", " <|> string "; ")
  pure case color of
    "red" -> (amount <= 12)
    "green" -> amount <= 13
    "blue" -> amount <= 14
    _ -> false

parseGame :: Parser String GameWithValidity
parseGame = do
  _ <- string "Game "
  gameNo <- intDecimal
  _ <- string ": "
  moves <- many parseMove
  _ <- char '\n'
  pure
      {
      id: gameNo,
      valid: (filter (\n -> n == false) moves # length) == 0
      }

partOne :: Effect Unit
partOne = do
  input <- readFile "src/day2input.txt"
  let
    games = case (runParser input (many parseGame)) of
      Right x -> x
      Left _ -> Nil
    validGames = filter (_.valid) games
    validIds = map (_.id) validGames
    idSum = foldl (+) (0) (listToArray validIds)
  logShow idSum

------------ Part two --------------

maxAmount ∷ ∀ (a119 ∷ Type) (t127 ∷ Row Type). Eq a119 ⇒ List { amount ∷ Int , color ∷ a119 | t127 } → a119 → Int
maxAmount list color = (filter (\item -> item.color == color) list) # map (_.amount) # maximum # intFromMaybe

parseAmounts :: Parser String Cube
parseAmounts = do
  amount <- intDecimal
  _ <- whiteSpace
  color <- string "green" <|> string "red" <|> string "blue"
  _ <- optional (string ", " <|> string "; ")
  pure {
    color: color,
    amount: amount
  }

parseGamePart2 :: Parser String GameWihtMinAmounts
parseGamePart2 = do
  _ <- string "Game "
  gameNo <- intDecimal
  _ <- string ": "
  cubes <- many parseAmounts
  _ <- char '\n'
  pure
      {
      id: gameNo,
      minAmount: (maxAmount cubes "red") * (maxAmount cubes "green") * (maxAmount cubes "blue")
      }

partTwo :: Effect Unit
partTwo = do
  input <- readFile "src/inputs/day2input.txt"
  let
    games = case (runParser input (many parseGamePart2)) of
      Right x -> x
      Left _ -> Nil
    minAmounts = map (_.minAmount) games
    idSum = foldl (+) (0) (listToArray minAmounts)
  logShow idSum