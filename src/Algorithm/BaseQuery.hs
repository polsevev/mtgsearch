{-# LANGUAGE OverloadedStrings #-}
module Algorithm.BaseQuery
    ( superType,
    Card(..)
    ) where
import qualified Data.Text as T
import Database.SQLite.Simple
import Config (getDbPath)
import Data.Text (Text, isInfixOf)
import Control.Monad.IO.Class (MonadIO(liftIO))
data Card = Card
  Int
  T.Text
  T.Text
  T.Text
  (Maybe T.Text)
  (Maybe T.Text)
  T.Text
  deriving (Show)

instance FromRow Card where
  fromRow = Card <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Card where
  toRow (Card id_ scryfall_id lang name oracle_text image_uri type_line) = toRow (id_, scryfall_id, lang, name, oracle_text, image_uri, type_line)


superType :: String -> IO [Card]
superType qry = do
  dbPath <- getDbPath
  conn <- open dbPath
  res <- query_ conn "select id, scryfall_id, lang, name, oracle_text, image_uri, type_line from card where type_line is not null" :: IO [Card];
  return $ typeLineFilter res (T.pack qry)

typeLineFilter :: [Card] -> Text -> [Card]
typeLineFilter (card@(Card _ _ _ _ _ _ type_line):cards) qry = if qry `isInfixOf` type_line then card:typeLineFilter cards qry else typeLineFilter cards qry

typeLineFilter [] _ = []