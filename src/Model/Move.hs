module Model.Move where

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
