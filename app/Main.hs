
module Main (main) where
import System.Environment   
import Site.Host
import Lib


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--seed"] -> seedData
        _ -> host
