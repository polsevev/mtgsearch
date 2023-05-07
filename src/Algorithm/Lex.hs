{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Lex
    ( lexx
    , Token(..)
    , QueryDef(..)
    , Operator(..)
    , ParseError(..)) where
import Prelude hiding (lex)
import Data.Either
import Text.Read (readMaybe)

lexx :: String -> Either ParseError Token
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
fixSeparators values parenthesis@((start,end):_) | start == 0 && end == ( length values -1) = (values, parenthesis)
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


newtype ParseError = ParseError String

data Token = 
    Func Operator Token Token | 
    Queri QueryDef 
    deriving Show
data QueryDef = 
    SuperType String|
    NotSuperType String |
    Color String | 
    CMCLT Int | 
    CMCMT Int | 
    CMCEQ Int |
    IsLegal String 
    deriving Show
data Operator = 
    Union |
    Intersect |
    Minus
    deriving Show

seperator :: [String] -> [(Int, Int)] -> Either ParseError Token

seperator ("(":rest) ((start,end):points) = case drop (end-start) rest of
            " ":operator:" ":"(":rightRest -> case spawnBranch (extractOperator operator) (seperator (take (end-start) rest) points)  (seperator (init rightRest) points) of
                Left a -> Left a
                Right b -> Right b
            [] -> seperator (take (end-start) rest) points
            a -> Left $ ParseError $ "Could not parse input, error happened at: " ++ (show (concat a))
seperator [name, " ", value, ")"] _ = case extractQueryDef (name, value) of 
    Left a -> Left a
    Right b -> Right $  b
seperator a _ = Left (ParseError ("Something went wrong tokenizing the input!\n" ++ (show a)))

spawnBranch :: Operator -> (Either ParseError Token) -> (Either ParseError Token) -> (Either ParseError Token)
spawnBranch _ (Left res1) _ = Left res1
spawnBranch _ _ (Left res2) = Left res2
spawnBranch operator (Right res1) (Right res2) = Right (Func operator res1 res2)




extractQueryDef :: (String, String) -> Either ParseError Token
extractQueryDef ("SuperType", value) = Right $ Queri $ SuperType value
extractQueryDef ("NotSuperType", value) = Right $ Queri $ NotSuperType value
extractQueryDef ("CmcLT", value) = case readMaybe value :: Maybe Int of 
    Just a -> Right $ Queri $ CMCLT a
    Nothing -> Left $ ParseError "Could not parse number from call to CmcLT"
extractQueryDef ("CmcMT", value) = case readMaybe value :: Maybe Int of 
    Just a -> Right $ Queri $ CMCMT a
    Nothing -> Left $ ParseError "Could not parse number from call to CmcMT"
extractQueryDef ("CmcEQ", value) = case readMaybe value :: Maybe Int of 
    Just a -> Right $ Queri $ CMCEQ a
    Nothing -> Left $ ParseError "Could not parse number from call to CmcEQ"
extractQueryDef ("CmcLTEQ", value) = case readMaybe value :: Maybe Int of
    Just a -> Right $ Func Union (Queri $ CMCLT a) (Queri $ CMCEQ  a) 
    Nothing -> Left $ ParseError "Could not parse number from call to CmcLTEQ"
extractQueryDef ("CmcMTEQ", value) = case readMaybe value :: Maybe Int of
    Just a -> Right $ Func Union (Queri $ CMCMT a) (Queri $ CMCEQ  a) 
    Nothing -> Left $ ParseError "Could not parse number from call to CmcLTEQ"
extractQueryDef ("Color", value) = Right $ Queri $ Color value
extractQueryDef ("IsLegal", value) = Right $ Queri $ IsLegal value 
extractQueryDef (a,b) = Left $ ParseError $ "The following command is invalid " ++ show a

extractOperator "union" = Union
extractOperator "intersect" = Intersect
extractOperator "minus" = Minus
extractOperator _ = error $ "This operator is not defined"
