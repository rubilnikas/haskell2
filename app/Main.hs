module Main where
import HttpClient.TicTacToeApiClient
import System.Environment
import Resolver.MoveResolver
import Model.Deserialization
import Model.Serialization
import Model.Move

main :: IO ()
main = do
    args <- getArgs
    case args of
      (id:"1":p:xs) -> atack id "1" (toPlayer p) (Just [])
      (id:"2":p:xs) -> wait id "2" (toPlayer p)
      _           -> putStrLn "Wrong number of arguments. id 1/2 X/O"

wait:: String -> String -> Player -> IO ()
wait id player p = do
   response <- getMoves id player
   let moves = deserialize response
   atack id player p moves

atack:: String -> String -> Player -> Maybe [Move] -> IO ()
atack _ _ _ Nothing = do
  putStrLn "Something realy bad happened. Moves can not be Nothing"
atack id player p (Just moves) = let
  board = fillBoard moves getEmptyBoard
  in case (winner board) of
    (Just X) -> putStrLn "X won"
    (Just O) -> putStrLn "Y won"
    _ -> case (resolve moves p) of
      Nothing   -> putStrLn "Draw"
      Just move -> do
        postMove id player (serialize (moves++[move]))
        wait id player p
