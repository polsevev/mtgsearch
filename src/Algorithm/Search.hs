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
import Algorithm.Operator (union, intersect)
import Control.Monad.IO.Class

data Tree = Funct Operator Tree Tree | Holder [Card]

search :: String -> IO String
search q = do
  let tokens = lexx q
  tree <- liftIO (executeBottomQuery tokens)

  let queryRes = executeQuery tree
  let hyperText = buildHtml queryRes
  return hyperText



executeQuery :: Tree -> [Card]
executeQuery (Holder cards) = cards
executeQuery (Funct Union leftToken rightToken) = executeQuery leftToken `union` executeQuery rightToken
executeQuery (Funct Intersect leftToken rightToken) = executeQuery leftToken `intersect` executeQuery rightToken
executeQuery _ = error $ "Not implemented!"


buildHtml :: [Card] -> String
buildHtml = concatMap cardToHtml


--Fancy trickery to move the IO to outer, in order to allow all the combinatorics to not have to live in IO land :)
executeBottomQuery :: Token -> IO Tree
executeBottomQuery (Queri (SuperType value)) =  do 
  temp <- superType value
  return $ Holder temp
executeBottomQuery (Queri _) = error $ "Not implemented yet"
executeBottomQuery (Func operator left right) = do 
  left <- executeBottomQuery left
  right <- executeBottomQuery right
  return $ Funct operator left right





cardToHtml :: Card -> String
cardToHtml (Card id_ scryfall_id lang name (Just oracle_text) (Just image_uri) type_line) =
  "<div class=\"card\" style=\"text-align:center\"><h2>" ++ unpack name ++ "</h2>" ++
  "<img src=" ++ unpack image_uri ++ " width=\"200px\"/>"++
  "<p>" ++ unpack oracle_text ++ "<p>"++" </div>"


cardToHtml _ = "<h1>Could not load that card</h1>"