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
import Algorithm.Operator (union, intersect, minus)
import Control.Monad.IO.Class
import Data.Aeson.Encoding (value)



search :: String -> IO String
search q = do
  let tokens = lexx q
  --In order to avoid IO when performing the operators, we fetch all the "bottom" queries first, then perform
  --the operators on them based on the Tree
  tree <- executeBottomQuery tokens

  let queryRes = executeQuery tree
  let hyperText = buildHtml queryRes
  return hyperText



executeQuery :: Tree -> [Card]
executeQuery (Holder cards) = cards
executeQuery (Funct Union leftToken rightToken) = executeQuery leftToken `union` executeQuery rightToken
executeQuery (Funct Intersect leftToken rightToken) = executeQuery leftToken `intersect` executeQuery rightToken
executeQuery (Funct Minus leftToken rightToken) = executeQuery leftToken `minus` executeQuery rightToken
executeQuery _ = error $ "Not implemented!"


buildHtml :: [Card] -> String
buildHtml = concatMap cardToHtml



executeBottomQuery :: Token -> IO Tree
executeBottomQuery (Queri (SuperType value)) = superType value
executeBottomQuery (Queri (CMCLT value)) = cmcLT value
executeBottomQuery (Queri (CMCMT value)) = cmcMT value
executeBottomQuery (Queri (CMCEQ value)) = cmcEQ value
executeBottomQuery (Queri _) = error $ "Not implemented yet"
executeBottomQuery (Func operator left right) = do 
  left <- executeBottomQuery left
  right <- executeBottomQuery right
  return $ Funct operator left right





cardToHtml :: Card -> String
cardToHtml (Card id_ scryfall_id lang name (Just oracle_text) (Just image_uri) type_line cmc) =
  "<div class=\"card\" style=\"text-align:center\"><h2>" ++ unpack name ++ "</h2>" ++
  "<img src=" ++ unpack image_uri ++ " width=\"200px\"/>"++
  "<p style=\"width:200px\">" ++ unpack oracle_text ++ "<p>"++" </div>"


cardToHtml _ = "<h1>Could not load that card</h1>"