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

    let collected = clearRepeatedSpaces $ collector qur
    let parenthesises = trace (show $ matchParenthesis collected 0) (matchParenthesis collected 0)
    let (parenthesisFixed, parenthesis) = trace (show $ fixSeparators collected parenthesises) fixSeparators collected parenthesises
    seperator parenthesisFixed parenthesis


fixSeparators :: [String] -> [(Int, Int)] -> ([String], [(Int, Int)])
fixSeparators values parenthesis@((start,end):rest) | start == 0 && end == ( length values -1) = (values, parenthesis)
fixSeparators values parenthesis  = ( ["("] ++ values ++ [")"], (0, length values + 1):map addOne parenthesis)



addOne (x,y) = (x+1, y+1)

isLegal :: Char -> Bool
isLegal x = x `notElem` ['(',')',' ']


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
data QueryDef = SuperType String| Color String deriving Show
data Operator = Union deriving Show

seperator :: [String] -> [(Int, Int)] -> Token
seperator ("(":rest) ((start,end):points) = case drop (end-start) rest of
            " ":operator:" ":"(":rightRest -> Func (extractOperator operator) (seperator (take (end-start) rest) points)   (seperator (init rightRest) points)
            [] -> seperator (take (end-start) rest) points
            _ ->error "Operator need something to the right and left of it!"
seperator [name, " ", value, ")"] _ = Queri (extractQueryDef (name, value))
seperator a _ = error $ "Something went wrong tokenizing the input!\n" ++ (show a) 

extractQueryDef :: (String, String) -> QueryDef
extractQueryDef ("SuperType", value) = SuperType value
extractQueryDef _ = error $ "This command was not valid"

extractOperator "union" = Union
extractOperator _ = error $ "This operator is not defined"
-- ((Is instant) union (Color R)) union ((Is instant) union (Color R))
-- (((Color Red) union (Color Blue)) union ((Is instant) union (Is Enchantment)))