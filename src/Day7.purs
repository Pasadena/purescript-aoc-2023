module Day7
  ( CardsInHand(..), CardValue(..)
  , day7partOne
  , day7partTwo
  )
  where

import Prelude

import Data.Array (filter, find, range, sort, uncons, zip)
import Data.Foldable (length, maximum, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (getOrElse, readFile)

newtype CardValue = CardValue Char

derive newtype instance showCardValue :: Show CardValue
derive newtype instance eqCardValue :: Eq CardValue

instance ordCardValue :: Ord CardValue where
  compare (CardValue a) (CardValue b) = compare (numericCardValue (CardValue a)) (numericCardValue (CardValue b))

joker :: CardValue
joker = CardValue 'W'

numericCardValue :: CardValue -> Int
numericCardValue (CardValue 'A') = 14
numericCardValue (CardValue 'K') = 13
numericCardValue (CardValue 'Q') = 12
numericCardValue (CardValue 'J') = 11
numericCardValue (CardValue 'T') = 10
numericCardValue (CardValue '9') = 9
numericCardValue (CardValue '8') = 8
numericCardValue (CardValue '7') = 7
numericCardValue (CardValue '6') = 6
numericCardValue (CardValue '5') = 5
numericCardValue (CardValue '4') = 4
numericCardValue (CardValue '3') = 3
numericCardValue (CardValue '2') = 2
numericCardValue (CardValue 'W') = 1
numericCardValue _ = -1

data HandType = FiveOfKind | FourOfKind | ThreeOfKind | TwoOfKind | TwoPairs | FullHouse | HighCard | Dummy

instance showHandType :: Show HandType where
  show (FiveOfKind) = "FiveOfKind"
  show (FourOfKind) = "FourOfKind"
  show (ThreeOfKind) = "ThreeOfKind"
  show (TwoOfKind) = "TwoOfKind"
  show (TwoPairs) = "TwoPairs"
  show (FullHouse) = "FullHouse"
  show (HighCard) = "HighCard"
  show (Dummy) = "Dummy"

typeStrength :: HandType -> Int
typeStrength FiveOfKind = 7
typeStrength FourOfKind = 6
typeStrength FullHouse = 5
typeStrength ThreeOfKind = 4
typeStrength TwoPairs = 3
typeStrength TwoOfKind = 2
typeStrength HighCard = 1
typeStrength Dummy = 0

data Hand = Hand {
  bid :: Int,
  cards :: CardsInHand,
  handType :: HandType
}
newtype CardsInHand = CardsInHand (Array CardValue)

derive newtype instance showCardsInHand :: Show CardsInHand

instance eqCardsInHand :: Eq CardsInHand where
  eq (CardsInHand a) (CardsInHand b) = do
    let
      zipped = zip a b
      firstDifference = find (\t -> (fst t) /= (snd t)) zipped
    case firstDifference of
      Just _ -> false
      Nothing -> true


instance ordCards :: Ord CardsInHand where
  compare (CardsInHand a) (CardsInHand b) = do
    let
      zipped = zip a b
      firstDifference = find (\t -> (numericCardValue (fst t)) /= (numericCardValue (snd t))) zipped
    case firstDifference of
      Just x -> case (fst x) > (snd x) of
        true -> GT
        false -> LT
      Nothing -> EQ

instance showHand :: Show Hand where
  show (Hand a) = "Hand => bid: " <> show a.bid <> ", type: " <> show a.handType <> ", cards: " <> show a.cards

instance eqHand :: Eq Hand where
  eq (Hand a) (Hand b) = a.cards == b.cards 

instance ordHand :: Ord Hand where
  compare (Hand a) (Hand b) = do
    let
      aStrength = typeStrength a.handType
      bStrength = typeStrength (b.handType)
    case (bStrength - aStrength) of
      0 -> compare a.cards b.cards
      _ -> case aStrength > bStrength of
        true -> GT
        false -> LT

countOccurances :: CardValue -> Array CardValue -> Int
countOccurances c chars = length $ filter (\y -> c == y) chars

parseHandType :: Array CardValue -> Boolean -> HandType
parseHandType chars jokersIncluded = do
  let jokerCount = if jokersIncluded then length $ filter (\c -> c == joker) chars else 0
  let charSet = if jokersIncluded then Set.fromFoldable (filter (\c -> c /= joker) chars) else Set.fromFoldable chars
  let charSetLength = length charSet
  let charCounts = Set.map (\x -> countOccurances x chars) charSet
  let maxAmount = fromMaybe (-1) (maximum charCounts)
  case max 1 charSetLength of
    1 -> FiveOfKind -- five cards that are the same
    2 -> case maxAmount + jokerCount of -- length 2 -> either 4 same and one random or 3 same and 2 same
      4 -> FourOfKind
      _ -> FullHouse
    3 -> case maxAmount + jokerCount of
      3 -> ThreeOfKind
      _ -> TwoPairs
    4 -> TwoOfKind -- 2 same and 3 random -> pair
    5 -> HighCard -- 5 different cards -> high card
    _ -> Dummy

parseHand :: String -> Boolean -> Hand
parseHand input jokersIncluded = do
  let parts = split (Pattern " ") input
  case uncons parts of
    Just {head: x, tail: rest} -> case uncons rest of
      Just { head: y, tail: _ } -> do
        let numericBid = getOrElse (fromString y) (-1)
        let chars = toCharArray x # map (\c -> if jokersIncluded && c == 'J' then joker else CardValue c)
        Hand {
          bid: numericBid,
          cards: (CardsInHand (chars)),
          handType: parseHandType chars jokersIncluded
        }
      Nothing -> Hand {
        bid: -1,
        cards: CardsInHand ([]),
        handType: HighCard
      }
    Nothing -> Hand {
      bid: -1,
      cards: CardsInHand [],
      handType: HighCard
    }

bidFromHand :: Hand -> Int
bidFromHand (Hand hand) = hand.bid

day7partOne :: Effect Unit
day7partOne = do
  input <- readFile "src/inputs/day7input.txt"
  let
    hands = lines input # map (\l -> parseHand l false)
    ranks = range 1 (length hands)
    sortedHands = sort hands
    bids = map (bidFromHand) sortedHands
    bidsAndRanks = zip (ranks) (bids)
    
    results = map (\tuple -> (fst tuple) * (snd tuple)) bidsAndRanks
  logShow (sum results)

---------------- PART 2 ----------------------------

day7partTwo :: Effect Unit
day7partTwo = do
  input <- readFile "src/inputs/day7input.txt"
  let
    hands = lines input # map \l -> parseHand l true
    ranks = range 1 (length hands)
    sortedHands = sort hands
    bids = map (bidFromHand) sortedHands
    bidsAndRanks = zip (ranks) (bids)
    
    results = map (\tuple -> (fst tuple) * (snd tuple)) bidsAndRanks
  logShow (sum results)
