module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Format
import qualified Data.Set as S
import Text.HTML.TagSoup
import System.Environment (getArgs)

import Types

-- | Break out conversations into message threads.
msgThreads :: [Tag T.Text] -> [[Tag T.Text]]
msgThreads html = f <$> sections (~== "<div class=\"thread\">") html
  where f x = init $ takeWhile (~/= "<div class=thread>") $ drop 1 x

msgMessages :: [Tag T.Text] -> [[Tag T.Text]]
msgMessages html = f <$> sections (~== "<div class=message>") html
  where f x = takeWhile (~/= "</p>") $ drop 1 x

parseMessage :: [Tag T.Text] -> Maybe Message
parseMessage msg =
  let info = map fromTagText $
             filter isTagText $
             head $
             sections (~== "<span class=user>") msg
  in if length info == 3
     then Just $ Message (head info) (info !! 1) (info !! 2)
     else Nothing

-- | Given a thread, pull out a list of participants.
msgParticipants :: [Tag T.Text] -> [T.Text]
msgParticipants = T.splitOn (T.pack ", ") . fromTagText . head . filter isTagText
{-# INLINE msgParticipants #-}

msgs :: String -> IO [Tag T.Text]
msgs filename =  fmap parseTags (T.readFile filename)
{-# INLINE msgs #-}

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  threads <- fmap msgThreads (msgs filename)
  let threadConvos =
        fmap (reverse . mapMaybe parseMessage . msgMessages) threads
  mapM_ process (zip [1..] threadConvos)
  where
    users d = S.toList . S.fromList $ fmap mUser d
    process (n, d) = do
      putStrLn $ "Processing " ++ show n ++ ".json: " ++ show (users d)
      L.writeFile ("/tmp/out/" ++ show n ++ ".json") (encode d)
      appendFile "/tmp/out/index" (show n ++ ".json: " ++ show (users d) ++ "\n")
