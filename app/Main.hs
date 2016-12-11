module Main where
import HttpClient.TicTacToeApiClient
import System.Environment
import Resolver.MoveResolver
import Model.Deserialization
import Model.Serialization
import Model.Move
import Text.Read

main :: IO ()
main = do
    args <- getArgs
    case args of
      (id:"1":p:lvl:xs) -> atack id "1" (toPlayer p) (Just []) (getLvlInt lvl)
      (id:"2":p:lvl:xs) -> wait id "2" (toPlayer p) (getLvlInt lvl)
      _           -> putStrLn "Wrong number of arguments. id 1/2 X/O 0-9 level"

wait:: String -> String -> Player -> Int -> IO ()
wait id player p lvl = do
   response <- getMoves id player
   let moves = deserialize response
   atack id player p moves lvl

atack:: String -> String -> Player -> Maybe [Move] -> Int -> IO ()
atack _ _ _ Nothing _ = do
  putStrLn "Something realy bad happened. Moves can not be Nothing"
atack id player p (Just moves) lvl = let
  board = fillBoard moves getEmptyBoard
  in case (winner board) of
    (Just X) -> putStrLn "X won"
    (Just O) -> putStrLn "Y won"
    _ -> case (resolve moves p lvl) of
      Nothing   -> putStrLn "Draw"
      Just move -> case winner $ fillBoard (moves++[move]) getEmptyBoard of
        (Just X) -> do
           postMove id player (serialize (moves++[move]))
           putStrLn "X won"
        (Just O) -> do
          postMove id player (serialize (moves++[move]))
          putStrLn "Y won"
        Nothing  -> case length (moves++[move]) of
          9 -> do
            postMove id player (serialize (moves++[move]))
            putStrLn "Draw"
          _ -> do
            postMove id player (serialize (moves++[move]))
            wait id player p lvl

getLvlInt:: String -> Int
getLvlInt s = case readMaybe s :: Maybe Int of
  Just lvl | lvl >= 0 && lvl < 10 -> lvl
  Nothing                         -> 3
