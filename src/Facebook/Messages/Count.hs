module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import System.Environment (getArgs)

import Types

msgs :: String -> IO [Message]
msgs jsonFile = do
  contents <- L.readFile jsonFile
  let j = decode contents :: Maybe [Message]
  return . fromJust $ j

main :: IO ()
main = do
  args <- getArgs
  let jsonFile = head args
  m <- msgs jsonFile
  putStrLn (outputGoogle $ groupedMsgs m)

parseMsgTime :: Message -> Day
parseMsgTime msg =
  parseTimeOrError True defaultTimeLocale "%A, %B %e, %0Y at %l:%M%P %Z" (T.unpack $ mTimestamp msg)
{-# INLINE parseMsgTime #-}

groupedMsgs :: [Message] -> [(Day, [Message])]
groupedMsgs msgs' =
  fmap f (groupBy ((==) `on` parseMsgTime) msgs')
  where
    f x = (parseMsgTime (head x), x)

dayToJS :: Day -> Int -> String
dayToJS day count =
  "[ new Date('" ++ show day ++ "'), " ++ show count ++ " ]"

outputGoogle :: [(Day, [Message])] -> String
outputGoogle msgs' =
  let dates = intercalate ", " (fmap (\(d, ms) -> dayToJS d (length ms)) msgs')
      users = S.toList . S.fromList $ fmap snd msgs' >>= \x -> fmap (T.unpack . mUser) x
  in
  "<!doctype html>\n\
  \<html>\
  \<head>\
  \  <meta charset=\"utf-8\" />\
  \  <script src=\"https://www.gstatic.com/charts/loader.js\"></script>\
  \  <script>\
  \    google.charts.load(\"current\", {packages:[\"calendar\"]});\
  \    google.charts.setOnLoadCallback(drawChart);\
  \ function drawChart() {\
  \     var dataTable = new google.visualization.DataTable();\
  \     dataTable.addColumn({ type: 'date', id: 'Date' });\
  \     dataTable.addColumn({ type: 'number', id: 'Messages' });\
  \     dataTable.addRows([" ++ dates ++ "\
  \      ]);\
  \     var chart = new google.visualization.Calendar(document.getElementById('calendar_basic'));\
  \     var options = {\
  \       title: \"Thread with: " ++ intercalate ", " users ++ "\",\
  \       height: 350,\
  \       noDataPattern: {\
  \         backgroundColor: '#aaa',\
  \         color: '#eee'\
  \       },\
  \     };\
  \     chart.draw(dataTable, options);\
  \ }\
  \  </script>\
  \</head>\
  \<body>\
  \  <div id=\"calendar_basic\" style=\"width: 1000px; height: 350px;\"></div>\
  \</body>\
  \</html>"
