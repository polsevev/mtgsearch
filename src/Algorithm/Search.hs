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
executeBottomQuery (Queri _) = error $ "Not implemented yet"
executeBottomQuery (Func operator left right) = do 
  left <- executeBottomQuery left
  right <- executeBottomQuery right
  return $ Funct operator left right





cardToHtml :: Card -> String
cardToHtml (Card _ _ _ name (Just cmc) (Just oracle_text) type_line (Just mana_cost) _) =
  "<div class=\"card\" style=\"text-align:center\"><h2>" ++ unpack name ++ "</h2>" ++
  "<p style=\"width:205px;margin: 0 auto;font-size:12;\">" ++ unpack oracle_text ++ "<p>"++" </div>"
cardToHtml _ = "<h1>Could not load that card</h1>"