module DeserializerSpec where

import Test.Hspec
import Test.QuickCheck
import Resolver.MoveResolver
import Model.Move
import Model.Deserialization
import Data.Maybe
import Data.List
import SpecHelper

deserializerSpec:: SpecWith ()
deserializerSpec = do
  describe "deserialization" $ do
    it "empty string -> nothing" $ do
       deserialize "" `shouldBe` (Nothing :: Maybe [Move])
    it "one move deserialize" $ do
      let input = "{\"0\":{\"x\":1,\"y\":1,\"v\":\"x\"}}"
      deserialize input `shouldBe` Just [Move 1 1 (Just X) 0]
