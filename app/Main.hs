
module Main (main) where
import System.Environment   

import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--seed"] -> seedData
        _ -> putStrLn "Run the main app"
