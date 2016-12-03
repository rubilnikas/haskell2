module Model.Move where

data Player = X | O
  deriving Show

data Move = Move{
     x :: Int
    ,y :: Int
    ,player :: Maybe Player
    ,rank :: Int
} deriving Show

instance Eq Move where
    (Move x1 y1 _ _) == (Move x2 y2 _ _) = x1 == x2 && y1 == y2
