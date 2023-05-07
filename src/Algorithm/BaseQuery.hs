{-# LANGUAGE OverloadedStrings #-}
module Algorithm.BaseQuery
    ( superType,
    Card(..),
    cmcLT,
    cmcMT,
    cmcEQ,
    Tree(..),
    CardFace(..),
    ImageUris(..),
    isLegal,
    notSuperType,
    color
    ) where
import qualified Data.Text as T
import Database.SQLite.Simple
import Config (getDbPath)
import Data.Text (Text, isInfixOf, unpack)
import Algorithm.Lex (Operator)
import Data.Maybe


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

--Re design fetching of cards by id to only gather the dbPath from config once!
fetchCardsWithIds :: [ID] -> IO [Card]
fetchCardsWithIds ids = do
  dbPath <- getDbPath
  conn <- open dbPath
  mapM (fetchCardWithId conn) ids

fetchCardWithId :: Connection -> ID -> IO Card
fetchCardWithId conn (ID id)  = do
  tempCardFaces <- runQueryNamed conn "select id, card_id, name, cmc, oracle_text, type_line, mana_cost from card_face where card_id = :val" [":val" := id] :: IO [TempCardFace]
  --We are fetchinig based on primary key, so we know the result is a single data
  [TempCard id scryfall_id lang name cmc oracle_text type_line mana_cost] <- runQueryNamed conn "select id, scryfall_id, lang, name, cmc, oracle_text,type_line, mana_cost from card where id = :val" [":val" := id] :: IO [TempCard]
  cardFaces <- mapM (fetchImageUris conn) tempCardFaces
  return $ Card id scryfall_id lang name cmc oracle_text type_line mana_cost cardFaces

fetchImageUris ::Connection -> TempCardFace -> IO (CardFace)
fetchImageUris conn (TempCardFace id card_id name cmc oracle_text type_line mana_cost) = do
  --We can do this only because a ImageUri row is 1:1 with CardFace in the database
  [imageUri] <- runQueryNamed conn "select id, card_face_id, small, normal, large, png, art_crop, border_crop from image_uris where card_face_id = :val" [":val" := id] :: IO [ImageUris]
  return $ CardFace id card_id name cmc oracle_text type_line mana_cost imageUri



runQuerySimple :: (FromRow a) => Connection ->  Query -> IO [a]
runQuerySimple conn str = do
  query_ conn str ::(FromRow a) =>  IO [a];

runQueryNamed ::(FromRow a) => Connection -> Query -> [NamedParam] -> IO [a]
runQueryNamed conn qur parm = do
  queryNamed conn qur parm :: (FromRow a) =>  IO [a]


--------------------------------------------------------------
data CardTypeLine = CardTypeLine
  Int
  T.Text

instance FromRow CardTypeLine where
  fromRow = CardTypeLine <$> field <*> field

superType :: String -> IO Tree
superType qry = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQuerySimple conn "select card.id, card_face.type_line from card inner join card_face where card.id = card_face.card_id and card_face.oracle_text is not null" :: IO [CardTypeLine]
  cards <- fetchCardsWithIds (typeLineFilter res (T.pack qry) True)
  return $ Holder cards

typeLineFilter :: [CardTypeLine] -> Text -> Bool -> [ID]
typeLineFilter ((CardTypeLine id_ type_line):cards) qry True = if qry `isInfixOf` type_line then ID id_:typeLineFilter cards qry True else typeLineFilter cards qry True
typeLineFilter ((CardTypeLine id_ type_line):cards) qry False = if not (qry `isInfixOf` type_line) then ID id_:typeLineFilter cards qry False else typeLineFilter cards qry False
typeLineFilter [] _ _ = []
--------------------------------------------------------------
notSuperType :: String -> IO Tree
notSuperType qry = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQuerySimple conn "select card.id, card_face.type_line from card inner join card_face where card.id = card_face.card_id and card_face.oracle_text is not null" :: IO [CardTypeLine]
  cards <- fetchCardsWithIds (typeLineFilter res (T.pack qry) False)
  return $ Holder cards


newtype ID = ID Int
instance FromRow ID where
  fromRow = ID <$> field

cmcLT :: Int -> IO Tree
cmcLT value = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQueryNamed conn "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc < :val or card_face.cmc < :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

cmcMT :: Int -> IO Tree
cmcMT value = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQueryNamed conn "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc > :val or card_face.cmc > :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

cmcEQ :: Int -> IO Tree
cmcEQ value = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQueryNamed conn "select card.id from card inner join card_face where card.id = card_face.card_id and (card.cmc = :val or card_face.cmc = :val)" [":val" := value] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

isLegal :: String -> IO Tree
isLegal qry = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- runQueryNamed conn "select card.id from card inner join legalities where card.id = legalities.card_id and format = :val and is_legal=1" [":val" := qry] :: IO [ID]
  cards <- fetchCardsWithIds res
  return $ Holder cards

-----------------------------------------------------

data CardColor = CardColor
  Int
  (Maybe Text)
  (Maybe Text)
instance FromRow CardColor where
  fromRow = CardColor <$> field <*> field <*> field
color :: String -> IO Tree
color qry = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <-  runQuerySimple conn "select card.id, card_face.mana_cost, card.mana_cost from card inner join card_face where card.id = card_face.card_id" :: IO [CardColor]
  let filtered = colorFilter qry res
  cards <- fetchCardsWithIds filtered
  return $ Holder cards

colorFilter :: String -> [CardColor] -> [ID]
colorFilter qry (card@(CardColor id card_mc face_mc):cards) = if all ((== True) . colorMatcher card) qry  then ID id :colorFilter qry cards else colorFilter qry cards
colorFilter qry [] = []


colorMatcher ::CardColor -> Char -> Bool
colorMatcher  (CardColor id card_mc face_mc) letter= letter `elem` unpack (fromMaybe "" card_mc) || letter `elem` unpack (fromMaybe "" face_mc)

-----------------------------------------------------