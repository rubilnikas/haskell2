{-# LANGUAGE OverloadedStrings #-}
module Model.Move where
import Data.Aeson

instance ToJSON Move where
    toJSON (Move x y (Just p) _) =
        object ["x" .= x, "y" .= y, "v" .= (toCharPlayer p)]

reverseP:: Player -> Player
reverseP X = O
reverseP O = X

reversePFromMaybe:: Maybe Player -> Player
reversePFromMaybe (Just X) = O
reversePFromMaybe (Just O) = X

data Player = X | O
  deriving (Show, Eq)

data Move = Move{
     x :: Int
    ,y :: Int
    ,player :: Maybe Player
    ,rank :: Int
} deriving Show

instance Eq Move where
    (Move x1 y1 p1 _) == (Move x2 y2 p2 _) = x1 == x2 && y1 == y2 && p1 == p2

instance Ord Move where
  (Move _ _ _ r1) <= (Move _ _ _ r2) = r1 <= r2

applyRank:: Move -> Int -> Move
applyRank (Move x y p _) rank = Move x y p rank

applyPlayer:: Move -> Player -> Move
applyPlayer (Move x y _ r) p = Move x y (Just p) r

applyPlayerMaybe:: Maybe Move -> Player -> Maybe Move
applyPlayerMaybe (Just (Move x y _ r)) p = Just (Move x y (Just p) r)
applyPlayerMaybe n _ = n

toCharPlayer:: Player -> String
toCharPlayer X = "X"
toCharPlayer O = "O"

toMaybePlayer:: String -> Maybe Player
toMaybePlayer "X" = Just X
toMaybePlayer "x" = Just X
toMaybePlayer "O" = Just O
toMaybePlayer "o" = Just O
toMaybePlayer _   = Nothing

toPlayer:: String -> Player
toPlayer "X" = X
toPlayer "x" = X
toPlayer "O" = O
toPlayer "o" = O

zeroOrPos:: [Move] -> Maybe Move
zeroOrPos [] = Nothing
zeroOrPos (h:tail) = case (rank h) of
  a | a >= 0 -> Just h
  _          -> zeroOrPos tail
negOrZero:: [Move] -> Maybe Move -> Maybe Move
negOrZero (h:[]) z = case z of
  Nothing -> Just h
  _       -> z
negOrZero (h:tail) z = case (rank h) of
    a | a < 0  -> Just h
    a | a == 0 -> negOrZero tail (Just h)
    _          -> negOrZero tail z
posOrZero:: [Move]  -> Maybe Move -> Maybe Move
posOrZero (h:[]) z = case z of
  Nothing -> Just h
  _       -> z
posOrZero (h:tail) z = case (rank h) of
    a | a > 0  -> Just h
    a | a == 0 -> posOrZero tail (Just h)
    _          -> posOrZero tail z
