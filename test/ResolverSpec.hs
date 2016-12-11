module ResolverSpec where

import Test.Hspec
import Test.QuickCheck
import Resolver.MoveResolver
import Model.Move
import Model.Deserialization
import Data.Maybe
import Data.List
import SpecHelper

resolverSpec:: SpecWith ()
resolverSpec = do
  describe "ResolverSpecs" $ do
    it "Play against self always draw" $ do
      let game = worker [] X (9,9)
      let inBoard = fillBoard game getEmptyBoard
      let win = winner inBoard
      win `shouldBe` (Nothing::Maybe Player)

    it "Play against self 9lvl vs 0lvl - 9 win" $ do
      let game = worker [] X (9,0)
      let inBoard = fillBoard game getEmptyBoard
      let win = winner inBoard
      win `shouldBe` (Just X)

    it "Play against self 9lvl vs 5lvl - 9 win" $ do
      let game = worker [] X (9,5)
      let inBoard = fillBoard game getEmptyBoard
      let win = winner inBoard
      win `shouldBe` (Just X)

    it "Play against self 9lvl vs 6lvl - draw" $ do
      let game = worker [] X (9,6)
      let inBoard = fillBoard game getEmptyBoard
      let win = winner inBoard
      win `shouldBe` (Just X)

    it "Correct resolves index of move x * 3 + y" $ do
      (getIndexOfMoveInBoard $ Move 2 1 Nothing 0) `shouldBe` 7

    it "Correct fills board with one move" $ do
      let move = Move 1 1 (Just X) 0
      let filledBoard = fillBoard [move] getEmptyBoard
      let myMovseInBoard = filter (\m -> m == move ) filledBoard
      length myMovseInBoard `shouldBe` 1
      filledBoard!!4 `shouldBe` move

    it "Correct fills board with many move" $ do
      let move1 = Move 1 2 (Just X) 0
      let move2 = Move 0 2 (Just O) 0
      let filledBoard = fillBoard [move1, move2] getEmptyBoard
      let myMovseInBoard = filter (\m -> m == move1 || m == move2 ) filledBoard
      length myMovseInBoard `shouldBe` 2
      filledBoard!!5 `shouldBe` move1
      filledBoard!!2 `shouldBe` move2

    it "Do defense" $ do
      let prevXMoves = generateMoves X [(1,0),(0,1),(2,0)]
      let prevOMoves = generateMoves O [(0,0),(0,2),(1,1)]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let posMoves = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) board
      let result = resolve prevMoves X 9
      result `shouldBe` Just (Move 2 2 (Just X) 0)

    it "Last move rank = 0" $ do
      let prevXMoves = generateMoves X [(0,1),(0,2),(1,0),(1,1)]
      let prevOMoves = generateMoves O [(0,0),(1,2),(2,0),(2,1)]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let m = (Move 2 2 Nothing 0)
      let rank = getRanked board Max X 9 m
      rank `shouldBe` 0

    it "Win move rank = 10" $ do
      let prevXMoves = generateMoves X [(0,0),(0,2),(1,2),(2,0)]
      let prevOMoves = generateMoves O [(0,1),(1,0),(1,1)]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let m = (Move 2 1 Nothing 0)
      let rank = getRanked board Max O 9 m
      rank `shouldBe` 10

    it "Draw move rank = 0" $ do
      let prevXMoves = generateMoves X [(0,0),(0,2),(1,2),(2,0)]
      let prevOMoves = generateMoves O [(0,1),(1,0),(1,1)]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let m = (Move 2 2 Nothing 0)
      let rank = getRanked board Max O 9 m
      rank `shouldBe` 0

    it "Lose move rank = -10" $ do
      let prevXMoves = generateMoves X [(0,2),(1,0),(2,0)]
      let prevOMoves = generateMoves O [(0,0),(2,1),(2,2)]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let posMoves = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) board
      let m = (Move 0 1 Nothing 0)
      let rank = getRanked board Max O 9 m
      rank `shouldBe` (-10)
