{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Search
    ( search
    ) where
import Database.SQLite.Simple
import Config (getDbPath)
import Web.Scotty.Internal.Types
import Control.Monad
import qualified Data.Text as T


data Card = Card Int T.Text T.Text deriving (Show)

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field

instance ToRow Card where

  toRow (Card id_ str image_uri) = toRow (id_, str, image_uri)

search :: String -> IO String
search q = do
    simpleSearch q

simpleSearch :: String -> IO String
simpleSearch q = do
    dbPath <- getDbPath
    conn <- open dbPath
    res <-  queryNamed conn "select id, name, image_uri from card where name like :name" [":name" := ("%"++q++"%")] :: IO [Card]
    let hyperText = buildHtml res
    return hyperText

buildHtml :: [Card] -> String
buildHtml = concatMap cardToHtml

cardToHtml :: Card -> String
cardToHtml (Card id name image_uri) = "<h2>" ++ show name ++ "</h2>" ++ "<img src=" ++ show image_uri ++ "/>"