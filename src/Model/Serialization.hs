module Model.Serialization
where

import Model.Move
import Data.Aeson
import Data.ByteString.Lazy.Char8

serialize:: [Move] -> String
serialize m = let
  content = "{" ++ (seriaizeMoves m 0 "") ++ "}"
  in content

seriaizeMoves:: [Move] -> Int -> String -> String
seriaizeMoves [] _ content = content
seriaizeMoves (m:tail) count content = seriaizeMoves tail (count + 1) $ content ++ show count ++ ":" ++ (unpack $ encode m)
