{-# LANGUAGE OverloadedStrings #-}
module Algorithm.BaseQuery
    ( superType,
    Card(..),
    cmcLT,
    cmcMT,
    cmcEQ,
    Tree(..)
    ) where
import qualified Data.Text as T
import Database.SQLite.Simple
import Config (getDbPath)
import Data.Text (Text, isInfixOf)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Algorithm.Lex (Operator)
import GHC.Generics (Generic)


{- 
In this module we handle the "bottom" requests, these are made using Joins with only the minimum amount of data needed 

Then the ID of the valid cards is used to fetch a full "Card" with all fields needed to show to the user
-}

data Tree = Funct Operator Tree Tree | Holder [Card]




data ImageUris = ImageUris
  Int
  Int
  T.Text
  T.Text
  T.Text
  T.Text
  T.Text
  T.Text

instance FromRow ImageUris where
  fromRow = ImageUris <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field


data TempCardFace = TempCardFace
  Int
  Int
  T.Text
  (Maybe Int)
  (Maybe T.Text)
  (Maybe T.Text)
  (Maybe T.Text)

instance FromRow TempCardFace where
  fromRow = TempCardFace <$> field <*> field <*> field <*> field <*> field <*> field <*> field


data TempCard = TempCard
  Int
  T.Text
  T.Text
  T.Text
  (Maybe Int)
  (Maybe T.Text)
  T.Text
  (Maybe T.Text)
instance FromRow TempCard where
  fromRow = TempCard <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data CardFace = CardFace
  Int
  Int
  T.Text
  (Maybe Int)
  (Maybe T.Text)
  (Maybe T.Text)
  (Maybe T.Text)
  ImageUris


data Card = Card
  Int
  T.Text
  T.Text
  T.Text
  (Maybe Int)
  (Maybe T.Text)
  T.Text
  (Maybe T.Text)
  [CardFace]


instance Eq Card where
  (==) (Card id_ _ _ _ _ _ _ _ _ ) (Card id2_ _ _ _ _ _ _ _ _ ) = id_ == id2_


fetchCardsWithIds :: [ID] -> IO [Card]
fetchCardsWithIds = mapM fetchCardWithId

fetchCardWithId :: ID -> IO Card
fetchCardWithId (ID id) = do
  tempCardFaces <- runQueryNamed "select id, card_id, name, cmc, oracle_text, type_line, mana_cost from card_face where card_id = :val" [":val" := id] :: IO [TempCardFace]
  --We are fetchinig based on primary key, so we know the result is a single data
  [TempCard id scryfall_id lang name cmc oracle_text type_line mana_cost] <- runQueryNamed "select id, scryfall_id, lang, name, cmc, oracle_text,type_line, mana_cost from card where id = :val" [":val" := id] :: IO [TempCard]
  cardFaces <- mapM fetchImageUris tempCardFaces
  return $ Card id scryfall_id lang name cmc oracle_text type_line mana_cost cardFaces

fetchImageUris :: TempCardFace -> IO (CardFace)
fetchImageUris (TempCardFace id card_id name cmc oracle_text type_line mana_cost) = do
  --We can do this only because a ImageUri row is 1:1 with CardFace in the database
  [imageUri] <- runQueryNamed "select id, card_face_id, small, normal, large, png, art_crop, border_crop from image_uris where card_face_id = :val" [":val" := id] :: IO [ImageUris]
  return $ CardFace id card_id name cmc oracle_text type_line mana_cost imageUri



runQuerySimple :: (FromRow a) =>  Query -> IO [a]
runQuerySimple str = do
  dbPath <- getDbPath
  conn <- open dbPath
  query_ conn str ::(FromRow a) =>  IO [a];

runQueryNamed ::(FromRow a) =>  Query -> [NamedParam] -> IO [a]
runQueryNamed qur parm = do
  dbPath <- getDbPath
  conn <- open dbPath
  queryNamed conn qur parm :: (FromRow a) =>  IO [a]


--------------------------------------------------------------
data CardTypeLine = CardTypeLine
  Int
  T.Text

instance FromRow CardTypeLine where
  fromRow = CardTypeLine <$> field <*> field

superType :: String -> IO Tree
superType qry = do
  res <- runQuerySimple "select card.id, card_face.oracle_text from card inner join card_face where card.id = card_face.card_id and card_face.oracle_text is not null" :: IO [CardTypeLine]
  cards <- fetchCardsWithIds (typeLineFilter res (T.pack qry))
  return $ Holder cards

typeLineFilter :: [CardTypeLine] -> Text -> [ID]
typeLineFilter (card@(CardTypeLine id type_line):cards) qry = if qry `isInfixOf` type_line then ID id:typeLineFilter cards qry else typeLineFilter cards qry
typeLineFilter [] _ = []
--------------------------------------------------------------

newtype ID = ID Int
instance FromRow ID where
  fromRow = ID <$> field

cmcLT :: Int -> IO Tree
cmcLT value = do
  res <- runQueryNamed "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc < :val or card_face.cmc < :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

cmcMT :: Int -> IO Tree
cmcMT value = do
  res <- runQueryNamed "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc > :val or card_face.cmc > :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

cmcEQ :: Int -> IO Tree
cmcEQ value = do
  res <- runQueryNamed "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc = :val or card_face.cmc = :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards


