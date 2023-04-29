{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Seed
    ( seedData
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import Data.Text

import qualified Data.ByteString.Lazy as B

import Database.SQLite.Simple

import Config

import GHC.Generics
data ImageUris = ImageUris {
    small :: Text,
    normal :: Text,
    large :: Text,
    png :: Text,
    art_crop :: Text,
    border_crop :: Text
}deriving (Show,Generic)
instance FromJSON ImageUris
instance ToJSON ImageUris

data Card = Card{
    object :: Text,
    id :: Text,
    lang :: Text,
    name :: Text,
    oracle_text :: Maybe Text,
    image_uris :: Maybe ImageUris,
    type_line :: Text
} deriving (Show,Generic)

instance FromJSON Card
instance ToJSON Card

instance ToRow Card where
    toRow (Card object id lang name oracle_text (Just (ImageUris _ image_link _ _ _ _ )) type_line) = toRow (id, lang, name, oracle_text, image_link, type_line)
    toRow (Card object id lang name oracle_text Nothing type_line) = toRow (id, lang, name, oracle_text, Nothing:: (Maybe Text), type_line)



getJSON :: String -> IO B.ByteString
getJSON file = B.readFile file

insertCards :: Connection -> [Card] -> IO()
insertCards conn ((card):cards) = do
    execute conn "INSERT INTO card (scryfall_id, lang, name, oracle_text, image_uri, type_line) VALUES (?,?,?,?,?,?)" card 
    insertCards conn cards
    return ()
insertCards conn [] = return ()

seedData :: IO ()
seedData = do
    -- Get JSON data and decode it
    
    seedData <- getDataSeedPath
    dbPath <- getDbPath
    d <- (eitherDecode <$> getJSON seedData) :: IO (Either String [Card])
    conn <- open dbPath
    execute_ conn "CREATE TABLE IF NOT EXISTS card (id INTEGER PRIMARY KEY, scryfall_id TEXT, lang TEXT, name TEXT, oracle_text TEXT, image_uri TEXT, type_line TEXT)"
    

    case d of
        Left err -> putStrLn err
        Right ps -> insertCards conn ps

