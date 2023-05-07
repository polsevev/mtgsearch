{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Operator
    ( union,
    intersect,
    minus
    ) where


union :: Eq a => [a] -> [a] -> [a]
union (a:as) bs | a `notElem` bs =a : union as bs 
union (a:as) bs = union as bs
union [] bs = bs

intersect :: Eq a => [a] -> [a] -> [a]
intersect (a:as) b = if a `elem` b then a: intersect as b else intersect as b
intersect [] _ = []

minus :: Eq a => [a] -> [a] -> [a]
minus (a:as) bs | a `notElem` bs = a:minus as bs
minus (_:as) bs = minus as bs
minus [] _ = []