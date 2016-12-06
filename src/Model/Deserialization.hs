module Model.Deserialization
where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Model.Move

parseQuotedString :: Parser String
parseQuotedString = do
    _ <- string "\""
    s <- many alphaNum
    _ <- string "\""
    return $ s

data Pair = Pair{
  key:: String,
  value:: JObject
} deriving Show

data JObject = JInt Int
  | JString String
  | JPair (String, JObject)
  | JList [JObject]
  deriving Show

parseJson :: Parser JObject
parseJson = parseMapAsList
  <|> parseJsonString
  <|> parseJsonInt
  <?> "end of line"

parseJsonString :: Parser JObject
parseJsonString = do
  s <- parseQuotedString
  return $ JString s

parseJsonInt :: Parser JObject
parseJsonInt = do
  d <- many1 digit
  return $ JInt $ read d

parseMapAsList :: Parser JObject
parseMapAsList = do
  _ <- string "{"
  l <- optionMaybe parseJsonPair
  t <- many commaSeparated
  _ <- string "}"
  return $ JList $ (maybeToList l) ++ t
  where
    commaSeparated = do
        _ <- string ","
        v <- parseJsonPair
        return v

parseJsonPair:: Parser JObject
parseJsonPair = do
    key   <- parseQuotedString
    _     <- string ":"
    value <- parseJson
    return $ JPair (key, value)

deserialize:: String -> Maybe [Move]
deserialize source = case (parse parseJson "" source) of
            Left err  -> Nothing
            Right xs  -> Just $ toMoves xs

toMoves:: JObject -> [Move]
toMoves (JList list) = map toMove list
  where
    toMove:: JObject -> Move
    toMove (JPair (_, JList jmove)) = let
      x = findInt jmove "x"
      y = findInt jmove "y"
      p = toMaybePlayer $ findString jmove "v"
      in Move x y p 0

    findInt:: [JObject] -> String -> Int
    findInt ((JPair (fname, JInt val)):tail) name = if fname == name
                                                    then val
                                                    else findInt tail name
    findInt (h:t) s = findInt t s

    findString:: [JObject] -> String -> String
    findString ((JPair (fname, JString val)):tail) name = if fname == name
                                                    then val
                                                    else findString tail name
    findString (h:t) s = findString t s
