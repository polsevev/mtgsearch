{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Seed
    ( seedDataFile, seedDataWeb
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import Data.Text

import qualified Data.ByteString.Lazy as B

import Database.SQLite.Simple

import Config

import GHC.Generics
import DataFetcher (fetchData)
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
    type_line :: Maybe Text,
    cmc :: Int
} deriving (Show,Generic)

instance FromJSON Card
instance ToJSON Card

instance ToRow Card where
    toRow (Card object id lang name oracle_text (Just (ImageUris _ image_link _ _ _ _ )) type_line cmc) = 
        toRow (id, lang, name, oracle_text, image_link, type_line, cmc)
    toRow (Card object id lang name oracle_text Nothing type_line cmc) = toRow (id, lang, name, oracle_text, Nothing:: (Maybe Text), type_line, cmc)

seedDataFile :: IO ()
seedDataFile = do
    -- Get JSON data and decode it

    d <- getDataSeedPath

    dat <- (eitherDecode <$> getJSON d) :: IO (Either String [Card])
    seedData dat

seedDataWeb :: IO ()
seedDataWeb = do
    dat <- (eitherDecode <$> getJSONWeb) :: IO (Either String [Card])
    seedData dat
    


seedData d = do
    dbPath <- getDbPath
    conn <- open dbPath
    execute_ conn "CREATE TABLE IF NOT EXISTS card (id INTEGER PRIMARY KEY, scryfall_id TEXT, lang TEXT, name TEXT, oracle_text TEXT, image_uri TEXT, type_line TEXT, cmc INT)"

    case d of
        Left err -> putStrLn err
        Right ps -> insertCards conn ps

getJSON :: String -> IO B.ByteString
getJSON = B.readFile

getJSONWeb :: IO B.ByteString
getJSONWeb = fetchData

insertCards :: Connection -> [Card] -> IO()
insertCards conn ((card):cards) = do
    execute conn "INSERT INTO card (scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc) VALUES (?,?,?,?,?,?,?)" card
    insertCards conn cards
    return ()
insertCards conn [] = return ()

