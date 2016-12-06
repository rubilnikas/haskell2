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
seriaizeMoves (m:tail) count content = let
  withCount = content ++ "\"" ++ show count ++ "\":"
  sep = getSeperator tail
  in seriaizeMoves tail (count + 1) $ withCount ++ (unpack $ encode m) ++ sep
  where
    getSeperator:: [a] -> String
    getSeperator [] = ""
    getSeperator _ = ","
