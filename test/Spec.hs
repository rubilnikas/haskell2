import Test.Hspec

import Test.QuickCheck
import Resolver.MoveResolver
import Model.Move
import Model.Deserialization
import Data.Maybe
import Data.List

main :: IO ()
main =  hspec $ do
  describe "deserialization" $ do
    it "empty string -> nothing" $ do
       deserialize "" `shouldBe` (Nothing :: Maybe [Move])
    it "one move deserialize" $ do
      let input = "{\"0\":{\"x\":1,\"y\":1,\"v\":\"x\"}}"
      deserialize input `shouldBe` Just [Move 1 1 (Just X) 0]


  describe "Resolver" $ do
    it "Works" $ do
      Move 1 1 Nothing 0 `shouldBe` (Move 1 1 Nothing 0)

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
      let prevXMoves = [Move 1 0 (Just X) 0, Move 0 1 (Just X) 0, Move 2 0 (Just X) 0]
      let prevOMoves = [Move 0 0 (Just O) 0, Move 0 2 (Just O) 0, Move 1 1 (Just O) 0]
      let prevMoves = prevXMoves ++ prevOMoves
      let board = fillBoard prevMoves getEmptyBoard
      let posMoves = filter (\mp -> (player mp) == (Nothing:: Maybe Player)) board
      let result = resolve prevMoves X
      result `shouldBe` Just (Move 2 2 (Just X) 0)
