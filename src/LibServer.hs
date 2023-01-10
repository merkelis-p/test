{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibServer(
GameState(..),
Coord(..),
calcOccupied,
isResultCorrect,
updateGameState
) where
import Data.Vector as V
import Types(Coord(..), Check (Check))
import Data.Yaml as Y
import Data.List as L


data GameState where
  GameState :: [Coord] -> GameState
  deriving Eq

calcOccupied :: (Coord -> Int) -> [Coord] -> Vector Int
calcOccupied fn (x:xs) = let arr = calcOccupied fn xs in
                       let index = fn x in
    arr // [(index, arr V.! index + 1)]
calcOccupied _ [] = V.replicate 10 0

isResultCorrect :: GameState -> GameState -> Bool
isResultCorrect (GameState a) (GameState b) = L.null (a \\ b) && L.null (b \\ a)

updateGameState :: GameState -> Check -> GameState
updateGameState (GameState prev) (Check (x:xs)) = case L.elemIndex x prev
  of Nothing -> updateGameState (GameState (x:prev)) (Check xs)
     Just index -> updateGameState (GameState (L.take index prev <> L.drop (index+1) prev)) (Check xs)
updateGameState prev _ = prev

instance FromJSON GameState where
  parseJSON Y.Null = pure $ GameState []
  parseJSON (Y.Array v) = GameState <$> traverse parseJSON (V.toList v)

instance FromJSON Coord where
    parseJSON (Y.Object o) = Coord <$> o .: "col" <*> o .: "row"

instance FromJSON Check where
  parseJSON (Y.Object o) = Check <$> o .: "coords"