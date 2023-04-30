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


data Tree = Funct Operator Tree Tree | Holder [Card]

data Card = Card
  Int
  T.Text
  T.Text
  T.Text
  (Maybe T.Text)
  (Maybe T.Text)
  T.Text
  Int
  deriving (Show)

instance Eq Card where
  (==) (Card id_ _ _ _ _ _ _ _) (Card id2_ _ _ _ _ _ _ _) = id_ == id2_

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Card where
  toRow (Card id_ scryfall_id lang name oracle_text image_uri type_line cmc) = toRow (id_, scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc)


runQuerySimple :: Query -> IO [Card]
runQuerySimple str = do
  dbPath <- getDbPath
  conn <- open dbPath
  query_ conn str :: IO [Card];

runQueryNamed :: Query -> [NamedParam] -> IO [Card]
runQueryNamed qur parm = do
  dbPath <- getDbPath
  conn <- open dbPath
  queryNamed conn qur parm :: IO [Card]


superType :: String -> IO Tree
superType qry = do
  res <- runQuerySimple "select id, scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc from card where type_line is not null"
  return $ Holder (typeLineFilter res (T.pack qry))

typeLineFilter :: [Card] -> Text -> [Card]
typeLineFilter (card@(Card _ _ _ _ _ _ type_line _):cards) qry = if qry `isInfixOf` type_line then card:typeLineFilter cards qry else typeLineFilter cards qry
typeLineFilter [] _ = []

cmcLT :: Int -> IO Tree
cmcLT value = do
  res <- runQueryNamed "select id, scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc from card where cmc < :val" [":val" := value]
  return $ Holder res

cmcMT :: Int -> IO Tree
cmcMT value = do
  res <- runQueryNamed "select id, scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc from card where cmc > :val" [":val" := value]
  return $ Holder res

cmcEQ :: Int -> IO Tree
cmcEQ value = do
  res <- runQueryNamed "select id, scryfall_id, lang, name, oracle_text, image_uri, type_line, cmc from card where cmc = :val" [":val" := value]
  return $ Holder res