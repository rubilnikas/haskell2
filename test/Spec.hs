import Test.Hspec

import Test.QuickCheck
import Resolver.MoveResolver
import Model.Move

main :: IO ()
main =  hspec $ do
  describe "Resolver" $ do
    it "Works" $ do
      Move 1 1 Nothing 0 `shouldBe` (Move 1 1 Nothing 0)
    it "Correct resolves index of move x * 3 + y" $ do
      (getIndexOfMoveInBoard $ Move 2 1 Nothing 0) `shouldBe` 7
