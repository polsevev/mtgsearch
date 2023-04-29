{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Search
    ( search
    ) where
import Database.SQLite.Simple
import Algorithm.BaseQuery
import Config (getDbPath)
import qualified Data.Text as T
import Algorithm.Lex
import Control.Monad
import Data.Text (unpack)
import Algorithm.Operator (union)
import Control.Monad.IO.Class



search :: String -> IO String
search q = do
  let tokens = lexx q
  queryRes <- executeQuery tokens
  let hyperText = buildHtml queryRes
  return hyperText



executeQuery :: Token -> IO [Card]
executeQuery (Queri bottom) = executeBottomQuery bottom
executeQuery (Func Union leftToken rightToken) = executeQuery leftToken `union` executeQuery rightToken
executeQuery _ = error $ "Not implemented!"


buildHtml :: [Card] -> String
buildHtml = concatMap cardToHtml



executeBottomQuery :: QueryDef -> IO [Card]
executeBottomQuery (SuperType value) = superType value
executeBottomQuery _ = error $ "Not implemented yet"




cardToHtml :: Card -> String
cardToHtml (Card id_ scryfall_id lang name (Just oracle_text) (Just image_uri) type_line) =
  "<div class=\"card\" style=\"text-align:center\"><h2>" ++ unpack name ++ "</h2>" ++
  "<img src=" ++ unpack image_uri ++ " width=\"200px\"/>"++
  "<p>" ++ unpack oracle_text ++ "<p>"++" </div>"


cardToHtml _ = "<h1>Could not load that card</h1>"