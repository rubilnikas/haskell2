module Resolver.MoveResolver where
import Model.Move
import Data.Maybe
import Data.Sequence
import Data.Foldable (toList)

getEmptyBoard:: [Move]
getEmptyBoard = [Move x y Nothing 0 | x <- [0 .. 2], y <- [0 .. 2]]

fillBoard:: [Move] -> [Move] -> [Move]
fillBoard [] board = board
fillBoard (h:tail) board =
  let
    (index) = getIndexOfMoveInBoard h
    (updatedBoard) = update index h $ fromList board
  in
    fillBoard tail $ toList updatedBoard

getIndexOfMoveInBoard :: Move -> Int
getIndexOfMoveInBoard move = (x move) * 3 + (y move)

resolve:: [Move] -> Maybe Move
resolve _ = Just $ Move 1 1 Nothing 0
