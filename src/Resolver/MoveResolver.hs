module Resolver.MoveResolver where
import Model.Move
import Data.Maybe
import Data.Sequence (fromList, update)
import Data.List (filter, minimum, maximum)
import Data.Foldable (toList)

resolve:: [Move] -> Player -> Maybe Move
resolve m p = let
  board = fillBoard m getEmptyBoard
  posMoves = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) board
  move = minimax board posMoves Max p 0
  in applyPlayerMaybe move p

minimax:: [Move] -> [Move] -> MiniMax -> Player -> Int -> Maybe Move
minimax _ [] _ _ _  = Nothing
minimax board ap mx p deep = let
  ranked = map (getRanked board mx p deep) ap
  in case (mx) of
    Max -> posOrZero ranked
    Min -> negOrZero ranked
  where
    getRanked:: [Move] -> MiniMax -> Player -> Int -> Move -> Maybe Move
    getRanked b mm p d m = let
      am = applyPlayer m p
      mb = fillBoard [am] b
      in case(winner mb) of
        Nothing -> let
          allPoss = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) mb
          in case(allPoss) of
            [] -> Just am
            _  -> minimax mb allPoss (reverseM mm) (reverseP p) (d+1)
        Just _ -> case(mm)of
          Min -> Just (applyRank am (-10))
          Max -> Just (applyRank am (10))
    negOrZero:: [Maybe Move] -> Maybe Move
    negOrZero (h:[]) = h
    negOrZero (h:tail) = case (h) of
      Nothing -> negOrZero tail
      Just mr -> case (rank mr) of
        a | a < 0 -> h
        _         -> negOrZero tail
    posOrZero:: [Maybe Move] -> Maybe Move
    posOrZero (h:[]) = h
    posOrZero (h:tail) = case (h) of
      Nothing -> posOrZero tail
      Just mr -> case (rank mr) of
        a | a > 0 -> h
        _         -> negOrZero tail


getEmptyBoard:: [Move]
getEmptyBoard = [Move x y Nothing 0 | x <- [0 .. 2], y <- [0 .. 2]]

fillBoard:: [Move] -> [Move] -> [Move]
fillBoard [] board = board
fillBoard (h:tail) board = let
  index = getIndexOfMoveInBoard h
  updatedBoard = update index h $ fromList board
  in fillBoard tail $ toList updatedBoard

getIndexOfMoveInBoard:: Move -> Int
getIndexOfMoveInBoard move = (x move) * 3 + (y move)

winner:: [Move] -> Maybe Player
winner board = case(map (\m -> player m) board) of
  [a, b, c, _, _, _, _, _, _] | isJust a && a == b && b == c -> a
  [_, _, _, a, b, c, _, _, _] | isJust a && a == b && b == c -> a
  [_, _, _, _, _, _, a, b, c] | isJust a && a == b && b == c -> a
  [a, _, _, b, _, _, c, _, _] | isJust a && a == b && b == c -> a
  [_, a, _, _, b, _, _, c, _] | isJust a && a == b && b == c -> a
  [_, _, a, _, _, b, _, _, c] | isJust a && a == b && b == c -> a
  [a, _, _, _, b, _, _, _, c] | isJust a && a == b && b == c -> a
  [_, _, a, _, b, _, c, _, _] | isJust a && a == b && b == c -> a
  _                                                          -> Nothing

data MiniMax = Min | Max
  deriving (Show, Eq)

reverseM:: MiniMax -> MiniMax
reverseM Min = Max
reverseM Max = Min
