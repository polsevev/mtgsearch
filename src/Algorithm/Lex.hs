{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Lex
    ( lexx
    , Token(..)) where
import Prelude hiding (lex)
import Debug.Trace
import Data.ByteString (count, putStr)

lexx :: String -> Token
lexx qur = do

    let collected = collector qur
    let parenthesisFixed = fixSeparators collected
    let parenthesis = matchParenthesis parenthesisFixed 0
    seperator parenthesisFixed parenthesis


fixSeparators :: [String] -> [String]
fixSeparators ("(":values) = "(":values
fixSeparators values = ["("] ++ values++ [")"]


isLegal :: Char -> Bool
isLegal x = x `notElem` ['(',')',' ']

collector :: String -> [String]
collector [] = []
collector (x:r) | not $ isLegal x = [x] : collector r
collector elements = case takeWhile isLegal elements of
    "" -> collector (dropWhile isLegal elements)
    b -> b : collector (dropWhile isLegal elements)


matchParenthesis :: [String] -> Int -> [(Int, Int)]
matchParenthesis ("(":rest) count = (count, findClosing rest 0 (count+1)):matchParenthesis rest (count+1)
matchParenthesis (a:rest) count = matchParenthesis rest (count+1)
matchParenthesis [] _ = []


findClosing :: [String] -> Int -> Int -> Int
findClosing (")":xs) stackCount count | stackCount == 0 = count
findClosing (")":xs) stackCount count = findClosing xs (stackCount-1) (count+1)
findClosing ("(":xs) stackCount count = findClosing xs (stackCount+1) (count+1)
findClosing (x:xs) stackCount count = findClosing xs stackCount (count+1)
findClosing [] _ _ = error "Unequal number of parenthesis"


data Token = Seperated String Token Token | Queri String deriving Show

seperator :: [String] -> [(Int, Int)] -> Token
seperator ("(":rest) ((start,end):points) = case drop (end-start) rest of
            " ":operator:" ":"(":rightRest -> Seperated operator (seperator (take (end-start) rest) points)   (seperator (init rightRest) points)
            [] -> seperator (take (end-start) rest) points
            _ ->error "Operator need something to the right and left of it!"



seperator [name, " ", value, ")"] points = Queri (name++" "++value)
seperator a _ = Queri "LOL" --error "Something went wrong tokenizing the input!"



--This one works, but not for lines!

-- seperator :: [String] -> [(Int, Int)] -> Seperated
-- seperator ("(":rest) ((start,end):points) = case drop (end-start) rest of
--             " ":operator:" ":"(":rightRest -> Seperated operator (seperator (take (end-start) rest) points)   (seperator (init rightRest) points)
--             [] -> seperator (take (end-start) rest) points 
--             a ->trace (show a) error "This is not allowed"




-- ((Is instant) union (Color R)) union ((Is instant) union (Color R))
-- (((Color Red) union (Color Blue)) union ((Is instant) union (Is Enchantment)))