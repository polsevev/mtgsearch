{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Config
    ( getDataSeedPath, getDbPath, getBulkDataLink
    ) where
import Data.Aeson
import Data.Text

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8 (pack)
import GHC.Generics
import Control.Exception (try)

import GHC.IO.Exception

import System.IO.Strict as L
import Data.ByteString.Builder (lazyByteString)


data Config = Config{
    dataseedpath :: String,
    dbPath :: String,
    bulkDataLink :: String
} deriving (Show,Generic)

instance FromJSON Config
instance ToJSON Config



configFile = "./Config/config.json"


getJSON :: IO B.ByteString
getJSON = do
    a <- try $ B.readFile configFile :: IO (Either IOException B.ByteString )
    case a of
        Right a -> return a
        Left b -> error $ "Could not load configuration file"



readConfig :: IO (Maybe Config)
readConfig = do
    result <- (eitherDecode <$> getJSON) :: IO (Either String Config)
    case result of
        Right conf ->
            return (Just conf)
        Left err -> do
            putStrLn err
            return Nothing

getConfig :: IO Config
getConfig = extract readConfig

getDataSeedPath ::IO String
getDataSeedPath = do
    dataseedpath <$> getConfig

getDbPath ::IO String
getDbPath = do
    dbPath <$> getConfig

getBulkDataLink :: IO (String)
getBulkDataLink = do
    bulkDataLink <$> getConfig

extract :: IO (Maybe a) -> IO a
extract = (>>= maybe (ioError $ userError "Could not read config!") return)
