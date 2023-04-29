{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Operator
    ( union,
    intersect
    ) where


union :: [a] -> [a] -> [a]
union res1 res2 = res1 ++ res2

intersect :: Eq a => [a] -> [a] -> [a]
intersect (a:as) b = if a `elem` b then a: intersect as b else intersect as b

intersect [] _ = []