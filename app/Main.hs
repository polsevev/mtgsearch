module Main (main) where
import System.Environment   
import Site.Host
import Seed ( seedData )
import Lib


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--seed"] -> seedData
        _ -> host
