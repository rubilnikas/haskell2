module Resolver.MoveResolver where
import Model.Move
import Data.Maybe
import Data.Sequence (fromList, update)
import Data.List (filter, minimum, maximum)
import Data.Foldable (toList)

resolve:: [Move] -> Player -> Int -> Maybe Move
resolve m p lvl = let
  board = fillBoard m getEmptyBoard
  posMoves = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) board
  move = minimax board posMoves Max p lvl
  in applyPlayerMaybe move p

minimax:: [Move] -> [Move] -> MiniMax -> Player -> Int -> Maybe Move
minimax _ [] _ _ _  = Nothing
minimax board ap mx p deep = let
  getRank = (getRanked board mx p deep)
  ranked = map (\rm -> applyRank rm $ getRank rm) ap
  in case (mx) of
    Max | (length board) == (length ap) -> zeroOrPos ranked
    Max                                 -> posOrZero ranked Nothing
    Min                                 -> negOrZero ranked Nothing

getRanked:: [Move] -> MiniMax -> Player -> Int -> Move -> Int
getRanked _ _ _ 0 _ = 0
getRanked b mm p d m = let
  am = applyPlayer m p
  mb = fillBoard [am] b
  nmm = reverseM mm
  in case(winner mb) of
    Nothing -> let
      allPoss = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) mb
      in case(allPoss) of
        [] -> 0
        _  -> let
          ranked = map (getRanked mb (reverseM mm) (reverseP p) (d-1)) allPoss
          in case nmm of
            Min -> minimum ranked
            Max -> maximum ranked
    Just _ -> case(mm)of
      Min -> -10
      Max -> 10

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
