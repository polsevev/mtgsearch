{-# LANGUAGE OverloadedStrings #-}
module Algorithm.Operator
    ( union
    ) where


union :: IO [a] -> IO [a] -> [a]
union res1 res2 = res1 ++ res2