module HttpClient.TicTacToeApiClient where
import Data.Maybe
import Network.HTTP
import Network.URI

getMoves:: String -> String -> IO (String)
getMoves id player = do
  let uri = fromJust $ parseURI $ createUrl id player
  let request = Request uri GET [(Header HdrAccept "application/json+map")] ""
  resp <- simpleHTTP request >>= fmap (take 100) . getResponseBody
  return resp

postMove:: String -> String -> String -> IO ()
postMove id player request = do
  putStrLn $ (id++player)++request

createUrl:: String -> String -> String
createUrl id player = ("http://tictactoe.homedir.eu/game/"++id)++("/player/"++player)
