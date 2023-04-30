module Main (main) where
import System.Environment   
import Site.Host
import Seed ( seedDataFile, seedDataWeb )
import Lib
import DataFetcher

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--seedFromFile"] -> seedDataFile
        ["--seedFromWeb"] -> seedDataWeb
        _ -> host
