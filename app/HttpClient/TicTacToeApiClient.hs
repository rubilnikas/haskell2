module HttpClient.TicTacToeApiClient where
import Data.Maybe
import Network.HTTP
import Network.URI

getMoves:: String -> String -> IO (String)
getMoves id player = do
  let uri = fromJust $ parseURI $ createUrl id player
  let request = Request uri GET [(Header HdrAccept "application/json+map")] ""
  resp <- simpleHTTP request >>= fmap (take 1000) . getResponseBody
  return resp

postMove:: String -> String -> String -> IO ()
postMove id player rbody = do
  putStrLn (rbody ++ "\n")
  let uri = fromJust $ parseURI $ createUrl id player
  let cl = (Header HdrContentLength (show $ length rbody))
  let ct = (Header HdrContentType "application/json+map")
  let request = Request uri POST [ct, cl] rbody
  resp <- simpleHTTP request >>= fmap (take 1000) . getResponseBody
  return ()

createUrl:: String -> String -> String
createUrl id player = ("http://tictactoe.homedir.eu/game/"++id)++("/player/"++player)
