module Day4
  ( day4partOne
  , day4partTwo
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (find, sum)
import Data.Int (pow)
import Data.List (List(..), concat, filter, index, intersect, length, modifyAt, range, slice, snoc, uncons)
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (listToArray, readFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, manyTill, sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal, space)

type Card = {
  winningNumbers:: List Int,
  hand:: List Int
}

type CardWithCopies = {
  id :: Int,
  matches :: Int,
  instances :: Int
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
  _ <- intDecimal
  _ <- string ":"
  _ <- many space
  winningNumbers <- manyTill parseNumberWithSpaces (string "|")
  _ <- many space
  hand <- many parseNumberWithSpaces
  pure {
    winningNumbers: winningNumbers,
    hand: hand
  }

intersectCards :: Card -> List Int
intersectCards card = intersect (card.winningNumbers) (card.hand)

pointsFromCard :: Card -> Int
pointsFromCard card = do
  let amount = length $ (intersectCards card)
  case amount of
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
-- TODO: finish this part 2 ---
parseCardWithCopies :: Parser String CardWithCopies
parseCardWithCopies = do
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
    matches: length $ (intersect winningNumbers hand),
    instances: 1
  }

-- concatLists :: List CardWithCopies -> List CardWithCopies -> List CardWithCopies
-- concatLists first second = case first of
--   Nil -> second
--   x:Nil -> (snoc second x)
--   x:xs -> do
--     let foo = (snoc second x)
--     -- let _ = spy ("ID ==> " <> show foo) foo
--     concatLists (xs) (foo)

-- prependCopies :: List CardWithCopies -> List CardWithCopies -> List CardWithCopies
-- prependCopies reversedCopies copiesAndOriginal = case reversedCopies of
--   Nil -> copiesAndOriginal
--   x:Nil -> x : copiesAndOriginal
--   x:xs -> do
--     let foo = x : copiesAndOriginal
--     -- let _ = spy ("ID ==> " <> show foo) foo
--     concatLists (xs) (foo)

-- insertCopies :: List CardWithCopies -> List CardWithCopies -> Int -> List CardWithCopies
-- insertCopies original withCopies limit = case uncons original of
--   Just { head: x, tail: xs} -> case x.matches of
--       0 -> do
--         let withOriginal = (snoc withCopies x)
--         let _ = spy ("ID without amount ==> " <> show x.id) x.id
--         insertCopies xs withOriginal 0
--       amount -> do
--           let copies = slice 0 amount xs
--           let copiesLen = length copies
--           let result = x : copies
--           let _ = spy ("ID ==> " <> show x.id) x.id
--           let _ = spy ("Amount ==> " <> show amount) amount
--           let _ = spy ("Copies ==> " <> show copiesLen) copiesLen
--           let copiesReverse = reverse copies
--           let originalAndCopies = (prependCopies copiesReverse xs)
--           let _ = spy ("Original w copies ==> " <> show originalAndCopies) originalAndCopies
--           let _ = spy ("New length ==> " <> show (length originalAndCopies)) length originalAndCopies
--           -- let _ = spy ("Copies ==> " <> show copies) (length copies)
--           if limit > 10 then result else insertCopies (prependCopies copiesReverse xs) result (limit + 1)
--   Nothing -> withCopies

updateCard :: CardWithCopies -> Int -> CardWithCopies
updateCard card instances = { id: card.id, matches: card.matches, instances: card.instances + instances}
  
updateCards :: List CardWithCopies -> List Int -> Int -> List CardWithCopies
updateCards cards indexRange amountToUpdate = case uncons indexRange of
  Just { head: x, tail: xs} -> do
    let
      instances = case (index cards x) of
        Just card -> card.instances
        Nothing -> 0
    
    let foo = (modifyAt (x) (\card -> (updateCard card amountToUpdate)) cards)
    -- let fooArray = listToArray foo
    -- let _ = spy ("after update") (listToArray fooArray)
    -- let _ = spy ("instances") instances
    case foo of 
      Just value -> updateCards value xs amountToUpdate
      Nothing -> updateCards cards xs amountToUpdate
  Nothing -> cards

handleCopies :: List CardWithCopies -> List CardWithCopies -> List CardWithCopies
handleCopies original withCopies = case uncons original of
  Just { head: x, tail: xs} -> case (find (\card -> card.id == x.id) withCopies ) of
      Nothing -> handleCopies xs withCopies
      Just card -> case card.matches of
        0 -> handleCopies xs withCopies
        amount -> do
          let _ = spy ("ID") card.id
          let _ = spy ("Amount") amount
          let _ = spy ("Range to update") (listToArray (range (card.id) (card.id + amount - 1)))
          let idRange = range (x.id) (x.id + amount - 1)
          -- let cardsToUpdate = slice (x.id -1) (x.id + amount) withCopies
          -- let updatedCards
          let updatedCards = updateCards withCopies idRange card.instances
          -- let _ = spy ("Updated cards") updatedCards
          -- traverse (\a -> log $ show a) updatedCards
          handleCopies xs updatedCards
  Nothing -> withCopies

replicateCard :: CardWithCopies -> Int -> List CardWithCopies
replicateCard card times = range 0 (times -1) # map (\_ -> card)

mergeLists :: List CardWithCopies -> List CardWithCopies -> List CardWithCopies
mergeLists first second = case uncons second of
  Just { head: x, tail: xs} -> mergeLists (snoc first x) xs
  Nothing -> first
  

handleCopies2 :: List CardWithCopies -> List CardWithCopies -> List CardWithCopies
handleCopies2 original withCopies = case uncons original of
  Just { head: x, tail: xs} -> case x.matches of
      0 -> handleCopies xs withCopies
      amount -> do
        let multiplier = filter (\card -> card.id == x.id) withCopies # length
        -- let _ = spy ("Multiplier") multiplier
        let _ = spy ("X") x
        -- let _ = spy ("Amount") amount
        let elementsToCopy = slice 0 (amount) xs
        -- let _ = spy ("Elelments to coy") (length elementsToCopy)
        let copies = elementsToCopy # map (\card -> replicateCard card multiplier) # concat
        -- let _ = spy ("Copies") (listToArray copies)
        let combined = (mergeLists withCopies copies)
        -- let _ = spy ("Combined") (listToArray combined)
        handleCopies2 xs combined
  Nothing -> withCopies

pointsFromCard2 :: CardWithCopies -> Int
pointsFromCard2 card = do
  case card.matches of
    0 -> 0
    1 -> 1
    2 -> 2
    x -> pow 2 (x - 1)

day4partTwo :: Effect Unit
day4partTwo = do
  input <- readFile "src/inputs/day4input.txt"
  let
    cards = case runParser input (parseCardWithCopies `sepBy` char '\n') of
      Right x -> x
      Left _ -> Nil
    withCopies = handleCopies (cards) (cards)
    pointSum = withCopies # map (\card -> card.instances * (pointsFromCard2 card)) # sum
  logShow withCopies
  logShow (length withCopies)
  logShow (pointSum)