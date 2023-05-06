{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Seed
    ( seedDataFile, seedDataWeb, DbCard(..)
    ) where

import Data.Aeson ( eitherDecode, FromJSON (parseJSON), ToJSON (toJSON), Value (Object), (.:), fromJSON, Key, (.:?) )
import Data.Text as T

import qualified Data.ByteString.Lazy as B

import Database.SQLite.Simple

import Config

import GHC.Generics
import DataFetcher (fetchData)
import Prelude hiding (id)
import Control.Exception (IOException, try, SomeException)
import Data.Aeson.KeyMap (keys, insert)
import Data.Aeson.Types (toJSONKeyKey, Parser)



data CardFace = CardFace {
    id :: Maybe Int,
    card_id :: Maybe Int,
    name:: Text,
    cmc::Maybe Float,
    oracle_text::Maybe Text,
    image_uris:: Maybe ImageUris,
    type_line :: Maybe Text,
    mana_cost:: Maybe Text
}deriving (Show,Generic)
instance FromJSON CardFace
instance ToJSON CardFace

data ImageUris = ImageUris {
    card_face_id :: Maybe Int,
    small :: Text,
    normal :: Text,
    large :: Text,
    png :: Text,
    art_crop :: Text,
    border_crop :: Text
}deriving (Show,Generic)
instance FromJSON ImageUris
instance ToJSON ImageUris

data Legalities =  Legalities{
    cardId :: Maybe Int,
    standard :: Text,
    future :: Text,
    historic::Text,
    gladiator::Text,
    pioneer::Text,
    explorer::Text,
    modern::Text,
    legacy::Text,
    pauper::Text,
    vintage::Text,
    penny::Text,
    commander::Text,
    brawl::Text,
    historicbrawl::Text,
    alchemy::Text,
    paupercommander::Text,
    duel::Text,
    oldschool::Text,
    premodern::Text,
    predh::Text
}deriving (Show,Generic)
instance FromJSON Legalities

data DbLegality = DbLegality Int Text Bool

data Card = Card{
    c_id :: Text,
    lang :: Text,
    c_name:: Text,
    layout:: Text,
    c_cmc:: Maybe Float,
    c_oracle_text:: Maybe Text,
    c_type_line :: Maybe Text,
    c_mana_cost:: Maybe Text,
    card_faces:: Maybe [CardFace],
    c_image_uris :: Maybe ImageUris,
    legalities :: Legalities
} deriving (Show,Generic)

instance FromJSON Card where
    parseJSON (Object v) = do
        c_name <- v.: "name"
        c_id <- v .: "id"
        lang <- v .: "lang"
        c_cmc <- v .:? "cmc"
        c_oracle_text <- v .:? "oracle_text"
        c_type_line <- v .:? "type_line"
        c_mana_cost <- v .:? "mana_cost"
        card_faces <- v .:? "card_faces"
        layout <- v .: "layout"
        c_image_uris <- v .:? "image_uris"
        legalities <- v .: "legalities"
        return (Card {
            c_id = c_id,
            lang = lang,
            c_name = c_name,
            c_cmc = c_cmc,
            c_oracle_text = c_oracle_text,
            c_type_line = c_type_line,
            c_mana_cost = c_mana_cost,
            card_faces = card_faces,
            layout = layout,
            c_image_uris = c_image_uris,
            legalities = legalities
            })
    parseJSON a = error $ "could not parse card " ++ (show a)

data DbCard = DbCard Int Text Text Text Text (Maybe Float) (Maybe Text) (Maybe Text) (Maybe Text)

instance ToRow DbLegality where
    toRow(DbLegality id format legalStatus) =
        toRow(id, format, legalStatus)

instance ToRow DbCard where
    toRow (DbCard id scryfall_id lang name layout cmc oracle_text type_line mana_cost) =
        toRow (id, scryfall_id, lang, name, cmc, oracle_text, type_line, mana_cost)


instance ToRow CardFace where
    toRow(CardFace c_id card_id name cmc oracle_text image_uris type_line mana_cost) =
        toRow (c_id, card_id, name, cmc, oracle_text, type_line, mana_cost)

instance ToRow ImageUris where
    toRow (ImageUris card_face_id small normal large png art_crop border_crop) =
        toRow (card_face_id, small, normal, large, png, art_crop, border_crop)

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



cardToDbCard :: Int -> Card -> DbCard
cardToDbCard id (Card scryfall_id lang c_name layout c_cmc c_oracle_text c_type_line c_mana_cost _ _ _) =
    DbCard id scryfall_id lang c_name layout c_cmc c_oracle_text c_type_line c_mana_cost


seedData d = do
    dbPath <- getDbPath
    conn <- open dbPath
    execute_ conn "DROP TABLE IF EXISTS card"
    execute_ conn "DROP TABLE IF EXISTS image_uris"
    execute_ conn "DROP TABLE IF EXISTS card_face"
    execute_ conn "DROP TABLE IF EXISTS legalities"
    execute_ conn "CREATE TABLE IF NOT EXISTS card (id INTEGER PRIMARY KEY, scryfall_id TEXT, lang TEXT, name TEXT, cmc INT, oracle_text TEXT, type_line TEXT, mana_cost TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS card_face (id INTEGER PRIMARY KEY,card_id INT, name TEXT, cmc INT, oracle_text TEXT, type_line TEXT, mana_cost TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS image_uris (id INTEGER PRIMARY KEY, card_face_id INT, small TEXT, normal TEXT, large TEXT, png TEXT, art_crop TEXT, border_crop TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS legalities (id INTEGER PRIMARY KEY,card_id INT, format TEXT, is_legal BOOL)"


    case d of
        Left err -> putStrLn err
        Right ps -> do
            collectedCards <- collectCards conn 0 0 ps
            fillDb conn collectedCards
            return ()






getJSON :: String -> IO B.ByteString
getJSON = B.readFile

getJSONWeb :: IO B.ByteString
getJSONWeb = fetchData

---------------------------------------------------------------------------------------------------------

collectCards :: Connection -> Int -> Int -> [Card] -> IO ([DbCard], [DbLegality], [(CardFace, ImageUris)])

--Skip illegal card types that are not relevant (These are difficult to parse as they contain non standard card formats)
collectCards conn card_id card_face_id ((Card _ _ _ layout _ _ _ _ _ _ _):cards) | unpack layout `Prelude.elem` ["scheme", "token", "double_faced_token", "emblem", "art_series", "vanguard", "host"] =
    collectCards conn card_id card_face_id cards

--This should in theory never happen, however it is here for full coverage
collectCards _ _ _ (card@(Card _ _ _ _ _ _ _ _ Nothing Nothing _):_) = do
    error $ "This cannot happen! No card can not have cardfaces as well as no image_uris on top level. Please fix ur data!"

--This will be ran when the card is single face (most cards)
collectCards conn card_id card_face_id (card@(Card _ _ c_name _ c_cmc c_oracle_text c_type_line c_mana_cost Nothing image_uris@(Just (ImageUris _ small normal large png art_crop border_crop)) legal):cards) = do
    let legalities = insertIdLegalities legal card_id
    let cardFace = CardFace (Just card_face_id) (Just card_id) c_name c_cmc c_oracle_text Nothing c_type_line c_mana_cost
    let imageUris = ImageUris (Just card_face_id) small normal large png art_crop border_crop
    let dbCard = cardToDbCard card_id card
    --insertRows conn dbCard legalities [(cardFace, imageUris)]
    (dbCards, legalitiesRest, cfius) <- collectCards conn (card_id + 1) (card_face_id + 1) cards
    return (dbCard:dbCards, legalities++legalitiesRest, (cardFace, imageUris):cfius)

--This will be ran when a card is multi face
collectCards conn card_id card_face_id (card@(Card _ _ _ _ _ _ _ _ (Just card_faces) Nothing legal):cards) = do
    let legalities = insertIdLegalities legal card_id
    let dbCard = cardToDbCard card_id card
    let cardFacesAndImageUris = addCardIdToCardFaces card_id card_face_id card_faces
    --insertRows conn dbCard legalities cardFacesAndImageUris
    (dbCards, legalitiesRest, cfius) <- collectCards conn (card_id + 1) (card_face_id + Prelude.length cardFacesAndImageUris+1) cards
    return (dbCard:dbCards, legalities++legalitiesRest, cardFacesAndImageUris ++ cfius)

--This will be ran when a card contains multiple cards on the front
collectCards conn card_id card_face_id (card@(Card _ _ _ _ _ _ _ _ (Just card_faces) (Just image_uris) legal):cards) = do
    let legalities = insertIdLegalities legal card_id
    let dbCard = cardToDbCard card_id card
    let cardFacesAndImageUris =   addCardIdAndImageUrisToCardFaces card_id card_face_id image_uris card_faces
    --insertRows conn dbCard legalities cardFacesAndImageUris
    (dbCards, legalitiesRest, cfius) <- collectCards conn (card_id + 1) (card_face_id + Prelude.length cardFacesAndImageUris+1) cards
    return (dbCard:dbCards, legalities++legalitiesRest, cardFacesAndImageUris ++ cfius)

collectCards _ _ _  [] = return ([],[],[])


-- Helper functions for collectCards

--God help me i know this is garbage
insertIdLegalities :: Legalities -> Int -> [DbLegality]
insertIdLegalities (Legalities Nothing a b c d e f g h i j k l m n o p q r s t) id =
    [DbLegality id "standard" (a=="legal"), DbLegality id "future" (b=="legal"), DbLegality id "historic" (c=="legal"), DbLegality id "gladiator" (d=="legal"), DbLegality id "pioneer" (e=="legal"), DbLegality id "explorer" (f=="legal"), DbLegality id "modern" (g=="legal"), DbLegality id "legacy" (h=="legal"), DbLegality id "pauper" (i=="legal"), DbLegality id "vintage" (j=="legal"), DbLegality id "penny" (k=="legal"), DbLegality id "commander" (l=="legal"), DbLegality id "brawl" (m=="legal"), DbLegality id "historicBrawl" (n=="legal"), DbLegality id "alchemy" (o=="legal"), DbLegality id "paupercommander" (p=="legal"), DbLegality id "duel" (q=="legal"), DbLegality id "oldschool" (r=="legal"), DbLegality id "premodern" (s=="legal"), DbLegality id "predh" (t=="legal")]
insertIdLegalities _ _ = error $ "Illegal id insert into data legality"



addCardIdAndImageUrisToCardFaces :: Int -> Int -> ImageUris -> [CardFace] -> [(CardFace, ImageUris)]
addCardIdAndImageUrisToCardFaces card_id card_face_id image_uri (card_face:faces) = addCardIdAndImageUrisToCardFace card_id card_face_id image_uri card_face : addCardIdAndImageUrisToCardFaces card_id (card_face_id+1) image_uri faces
addCardIdAndImageUrisToCardFaces _ _ _ [] = []


addCardIdAndImageUrisToCardFace :: Int -> Int ->  ImageUris -> CardFace -> (CardFace, ImageUris)
addCardIdAndImageUrisToCardFace card_id card_face_id  (ImageUris _ small normal large png art_crop border_crop) (CardFace Nothing Nothing c_name c_cmc c_oracle_text Nothing c_type_line c_mana_cost) =
    (CardFace (Just card_face_id) (Just card_id) c_name c_cmc c_oracle_text Nothing c_type_line c_mana_cost, ImageUris (Just card_face_id) small normal large png art_crop border_crop)
addCardIdAndImageUrisToCardFace _ _ _ _ = error $ "This cardface and imageUris are not compatible"


addCardIdToCardFace :: Int -> Int -> CardFace -> (CardFace,ImageUris)
addCardIdToCardFace card_id card_face_id (CardFace Nothing Nothing c_name c_cmc c_oracle_text (Just (ImageUris i_card_face_id small normal large png art_crop border_crop)) c_type_line c_mana_cost) =
    (CardFace (Just card_face_id) (Just card_id) c_name c_cmc c_oracle_text Nothing c_type_line c_mana_cost, ImageUris (Just card_face_id) small normal large png art_crop border_crop)
addCardIdToCardFace _ _ _ = error $ "This cardface already has an id associated with it!"


addCardIdToCardFaces :: Int -> Int -> [CardFace] -> [(CardFace, ImageUris)]
addCardIdToCardFaces card_id card_face_id (card:cards) = addCardIdToCardFace card_id card_face_id card : addCardIdToCardFaces card_id (card_face_id +1) cards
addCardIdToCardFaces _ _ [] = []

---------------------------------------------------------------------------------------------------------------------------------------------------------------------

fillDb :: Connection -> ([DbCard], [DbLegality], [(CardFace, ImageUris)]) -> IO ()
fillDb conn (dbCards, legalities, combo) = do
    let (cardFaces, imageUris) = unzip combo
    executeMany conn "INSERT INTO card (id, scryfall_id, lang, name, cmc, oracle_text, type_line, mana_cost ) VALUES (?,?,?,?,?,?,?,?)" dbCards
    executeMany conn "INSERT INTO card_face (id, card_id, name, cmc, oracle_text, type_line, mana_cost) VALUES (?,?,?,?,?,?,?)" cardFaces
    executeMany conn "INSERT INTO image_uris (card_face_id, small , normal , large , png , art_crop, border_crop) VALUES (?,?,?,?,?,?,?)" imageUris
    executeMany conn "INSERT INTO legalities (card_id, format, is_legal) VALUES (?,?,?)" legalities
