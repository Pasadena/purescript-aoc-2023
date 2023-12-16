module Lib (
  intFromMaybe
  , readFile
  , listToArray
  , getOrElse
) where

import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readFile :: String -> Effect String
readFile fileName = readTextFile UTF8 fileName

intFromMaybe :: Maybe Int -> Int
intFromMaybe value = case value of
  Just x -> x
  Nothing -> 999999

listToArray ∷ ∀ (f164 ∷ Type -> Type) (a168 ∷ Type). Foldable f164 ⇒ f164 a168 → Array a168
listToArray list = case fromFoldable list of
  Just x -> toArray x
  Nothing -> []

getOrElse :: Maybe Int -> Int -> Int
getOrElse value default = case value of
  Just x -> x
  Nothing -> default