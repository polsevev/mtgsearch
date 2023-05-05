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
import Data.Char (toLower)
import qualified Data.Maybe

--Probably a lot better way to do this, ex: reading from the database, but i want to avoid having IO everywhere
formats = ["standard", "future", "historic", "gladiator", "pioneer", "explorer", "modern", "legacy", "pauper", "vintage", "penny", "commander", "brawl", "historicbrawl", "alchemy", "paupercommander", "duel", "oldschool", "premodern", "predh"]

search :: String -> IO String
search q = do
  case lexx q of
      Left (ParseError message) -> return message
      Right tokens ->do
        tree <- executeBottomQuery tokens
        let queryRes = executeQuery tree
        let hyperText = buildHtml queryRes
        return hyperText
  --In order to avoid IO when performing the operators, we fetch all the "bottom" queries first, then perform
  --the operators on them based on the Tree




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
executeBottomQuery (Queri (IsLegal value)) | map toLower value `elem` formats = isLegal $ map toLower value
executeBottomQuery (Queri _) = error $ "Not implemented yet"
executeBottomQuery (Func operator left right) = do
  left <- executeBottomQuery left
  right <- executeBottomQuery right
  return $ Funct operator left right


cardToHtml :: Card -> String
--Card with a single face!
cardToHtml (Card _ _ _ _ _ _ _ _ [cardFace]) = singleCardFaceHTML cardFace

--Multiface card!
cardToHtml (Card _ _ _ _ _ _ _ _ cardFaces) = "<div style=\"text-align:center;\"><div style=\"display: inline-flex\">" ++ concatMap singleCardFaceHTML cardFaces ++"</div></div>"

singleCardFaceHTML :: CardFace -> String
singleCardFaceHTML (CardFace _ _ name cmc oracle_text type_line mana_cost (ImageUris _ _ _ image _ _ _ _)) =
  "<div style=\"text-align:center;\">" ++
    "<h2>" ++ unpack name ++ "</h2>" ++
    "<img src=" ++ unpack image ++ " width=\"200px\"/>"++
    "<p style=\"width:205px;margin: 5 auto;font-size:16;\">" ++  unpack (Data.Maybe.fromMaybe "" type_line) ++ "</p>"++
    "<p style=\"width:205px;margin: 5 auto;font-size:12;\">" ++ unpack (Data.Maybe.fromMaybe "" oracle_text) ++ "</p>"++
    "<p style=\"width:205px;margin: 5 auto;font-size:16;\">Mana cost: " ++  filter (`notElem` ['{','}']) (unpack (Data.Maybe.fromMaybe "" mana_cost)) ++ "</p>"++
  " </div>"


parseCMC :: Maybe Int -> String
parseCMC (Just a) = show a
parseCMC Nothing = ""