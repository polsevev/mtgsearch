{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib
    ( seedData
    ) where
import Data.Aeson
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
    image_uris :: Maybe ImageUris
} deriving (Show,Generic)

instance FromJSON Card
instance ToJSON Card

instance ToRow Card where
    toRow (Card object id lang name oracle_text (Just (ImageUris _ image_link _ _ _ _ ))) = toRow (id, lang, name, oracle_text, image_link)
    toRow (Card object id lang name oracle_text Nothing) = toRow (id, lang, name, oracle_text, pack "Nothing")



getJSON :: String -> IO B.ByteString
getJSON file = B.readFile file

insertCards :: Connection -> [Card] -> IO()
insertCards conn ((card):cards) = do
    execute conn "INSERT INTO card (scryfall_id, lang, name, oracle_text, image_uri) VALUES (?,?,?,?,?)" card 
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
    execute_ conn "CREATE TABLE IF NOT EXISTS card (id INTEGER PRIMARY KEY, scryfall_id TEXT, lang TEXT, name TEXT, oracle_text TEXT, image_uri TEXT)"
    

    case d of
        Left err -> putStrLn err
        Right ps -> insertCards conn ps

