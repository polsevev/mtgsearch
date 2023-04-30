{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Lex
    ( lexx
    , Token(..)
    , QueryDef(..)
    , Operator(..)) where
import Prelude hiding (lex)
import Debug.Trace
import Data.ByteString (count, putStr)


lexx :: String -> Token
lexx qur = do
    let collected = clearRepeatedSpaces $ collector $ strip $ stripFront $ clearIllegalCharacters qur
    let parenthesises = matchParenthesis collected 0
    let (parenthesisFixed, parenthesis) = fixSeparators collected parenthesises
    seperator parenthesisFixed parenthesis

stripFront :: [Char] -> [Char]
stripFront = dropWhile (==' ')

strip :: String -> String
strip (' ':as) = case strip as of
    [] -> []
    _ -> ' ' : strip as
strip (a:as) = a : strip as
strip [] = []



fixSeparators :: [String] -> [(Int, Int)] -> ([String], [(Int, Int)])
fixSeparators values parenthesis@((start,end):rest) | start == 0 && end == ( length values -1) = (values, parenthesis)
fixSeparators values parenthesis  = ( ["("] ++ values ++ [")"], (0, length values + 1):map addOne parenthesis)


addOne (x,y) = (x+1, y+1)

isLegal :: Char -> Bool
isLegal x = x `notElem` ['(',')',' ']


clearIllegalCharacters :: String -> String
clearIllegalCharacters (c:cs) | c `elem` ['\n', '\r'] = ' ':clearIllegalCharacters cs
clearIllegalCharacters (c:cs) = c:clearIllegalCharacters cs
clearIllegalCharacters [] = []

clearRepeatedSpaces :: [String] -> [String]
clearRepeatedSpaces (a:as) = case a of
    " " -> " ": clearRepeatedSpaces (dropWhile (==" ") as)
    b -> b: clearRepeatedSpaces as
clearRepeatedSpaces [] = []


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


data Token = Func Operator Token Token | Queri QueryDef deriving Show
data QueryDef = 
    SuperType String|
    Color String | 
    CMCLT Int | 
    CMCMT Int | 
    CMCEQ Int 
    deriving Show
data Operator = 
    Union |
    Intersect |
    Minus
    deriving Show

seperator :: [String] -> [(Int, Int)] -> Token
seperator ("(":rest) ((start,end):points) = case drop (end-start) rest of
            " ":operator:" ":"(":rightRest -> Func (extractOperator operator) (seperator (take (end-start) rest) points)   (seperator (init rightRest) points)
            [] -> seperator (take (end-start) rest) points
            _ ->error "Operator need something to the right and left of it!"
seperator [name, " ", value, ")"] _ = Queri (extractQueryDef (name, value))
seperator a _ = error $ "Something went wrong tokenizing the input!\n" ++ (show a) 

extractQueryDef :: (String, String) -> QueryDef
extractQueryDef ("SuperType", value) = SuperType value
extractQueryDef ("CmcLT", value) = CMCLT (read value :: Int)
extractQueryDef ("CmcMT", value) = CMCMT (read value :: Int)
extractQueryDef ("CmcEQ", value) = CMCEQ (read value :: Int)
extractQueryDef _ = error $ "This command was not valid"

extractOperator "union" = Union
extractOperator "intersect" = Intersect
extractOperator "minus" = Minus
extractOperator _ = error $ "This operator is not defined"
-- ((Is instant) union (Color R)) union ((Is instant) union (Color R))
-- (((Color Red) union (Color Blue)) union ((Is instant) union (Is Enchantment)))