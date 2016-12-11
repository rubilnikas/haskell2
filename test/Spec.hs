import Test.Hspec

import ResolverSpec
import DeserializerSpec

main :: IO ()
main =  hspec $ do
  deserializerSpec
  resolverSpec
