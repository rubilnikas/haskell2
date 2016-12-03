module Model.Deserialization
where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Model.Move

data Pair = Pair{
  key:: String,
  value:: JObject
} deriving Show

data JObject = JInt Integer
  | JChar Char
  | JString String
  | JPair (String, JObject)
  | JList [JObject]
  deriving Show

parseJson :: Parser JObject
parseJson = parseMapAsList
  <|> parseJsonInt
  <|> parseJsonPair
  <|> parseJsonString
  <|> parseJsonChar
  <?> "end of line"

parseJsonChar :: Parser JObject
parseJsonChar = do
  s <- alphaNum
  return $ JChar s

parseJsonString :: Parser JObject
parseJsonString = do
  s <- many1 alphaNum
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
    key   <- many1 alphaNum
    _     <- string ":"
    value <- parseJson
    return $ JPair (key, value)


{-
parseMove :: Parser Val
parseMove = do
  _
  _    <- string "{"
  let move = Move 1 1 $ Just X
  _    <- string "}"
  return $ MoveVal move


parseJsonMovesMapAsList :: Parser Val
parseJsonMovesMapAsList = do
    _ <- string "{"
    l <- many parseMove
    _ <- string "}"
    return $ ListOfMovesVals l

deserialize:: String -> [Move]
deserialize _ = [Move 1 1 $ Just X]-}
