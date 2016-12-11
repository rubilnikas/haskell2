module SpecHelper where

import Resolver.MoveResolver
import Model.Move
import Model.Deserialization
import Data.Maybe
import Data.List

worker :: [Move] -> Player -> (Int, Int) -> [Move]
worker moves p (l1,l2) = case winner (fillBoard moves getEmptyBoard) of
  Nothing -> let
    maybeMOve = resolve moves p l1
    in case maybeMOve of
      Just move -> worker (move:moves) (reverseP p) (l2,l1)
      Nothing   -> moves
  _       -> fillBoard moves getEmptyBoard

generateMoves:: Player -> [(Int,Int)] -> [Move]
generateMoves p xy = map (\(x,y) -> Move x y (Just p) 0) xy
