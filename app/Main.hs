module Main where
import HttpClient.TicTacToeApiClient
import System.Environment

import Text.Parsec
import Text.Parsec.String


data Val = IntVal Integer
  | StringVal String
  | ListOfVals [Val]
  deriving Show

parseBencode :: Parser Val
parseBencode = parseBencodeInt
  <|> parseBencodeList
  <|> parseBencodeStr

parseBencodeList :: Parser Val
parseBencodeList = do
    _ <- string "l"
    l <- many parseBencode
    _ <- string "e"
    return $ ListOfVals l

parseBencodeStr :: Parser Val
parseBencodeStr = do
    l <- many1 digit
    _ <- string ":"
    s <- count (read l) anyChar
    return $ StringVal s

parseBencodeInt :: Parser Val
parseBencodeInt = do
    _ <- string "i"
    i <- parseIntVal
    _ <- string "e"
    return i

parseIntVal :: Parser Val
parseIntVal = (many1 digit >>=
  (\v -> return (IntVal (read v))))



main :: IO ()
main = do
    args <- getArgs
    case args of
      (id:"1":xs) -> atack id "1" ""
      (id:"2":xs) -> wait id "2"
      _           -> putStrLn "Wrong number of arguments"

wait:: String -> String -> IO ()
wait id player = do
   moves <- getMoves id player
   atack id player moves

atack:: String -> String -> String -> IO ()
atack id player moves = do
   mm <- getMoves id player
   putStrLn mm


{-
main = do
  putStrLn $ getMoves "myId" "player"
  putStrLn $ postMove "myId" "player" "BODY"

  putStrLn "Press any key to exit..."
  a <- getChar
  putStrLn $ a:[]-}
