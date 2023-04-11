{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Search
    ( search
    ) where
import Database.SQLite.Simple
import Config (getDbPath)
import Web.Scotty.Internal.Types
import Control.Monad
import qualified Data.Text as T


data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

search :: String -> IO String
search q = do
    simpleSearch q

simpleSearch :: String -> IO String
simpleSearch q = do
    dbPath <- getDbPath
    conn <- open dbPath
    res <-  queryNamed conn "select id, name from card where name like :name" [":name" := ("%"++q++"%")] :: IO [TestField]
    return (show res)