{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib
    ( seedData
    ) where
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import           Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple

import Config

import GHC.Generics
import GHC.Read (readField)

data Card = Card{
    object :: Text,
    id :: Text,
    lang :: Text,
    name :: Text,
    oracle_text :: Maybe Text
} deriving (Show,Generic)

instance FromJSON Card
instance ToJSON Card

instance ToRow Card where
  toRow (Card object id lang name oracle_text) = toRow (id, lang, name, oracle_text)



getJSON :: String -> IO B.ByteString
getJSON file = B.readFile file

insertCards :: Connection -> [Card] -> IO()
insertCards conn ((card):cards) = do
    execute conn "INSERT INTO card (scryfall_id, lang, name, oracle_text) VALUES (?,?,?,?)" card 
    insertCards conn cards
    return ()
insertCards conn [] = return ()

seedData :: IO ()
seedData = do
    -- Get JSON data and decode it
    
    seedData <- getDataSeedPath
    dbPath <- getDbPath
    d <- (eitherDecode <$> getJSON  seedData) :: IO (Either String [Card])
    conn <- open dbPath
    execute_ conn "CREATE TABLE IF NOT EXISTS card (id INTEGER PRIMARY KEY, scryfall_id TEXT, lang TEXT, name TEXT, oracle_text TEXT)"
    
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case d of
        Left err -> putStrLn err
        Right ps -> insertCards conn ps

