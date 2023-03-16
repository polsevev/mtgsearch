{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Config
    ( getDataSeedPath, getDbPath
    ) where
import Data.Aeson
import Data.Text

import qualified Data.ByteString.Lazy as B
import GHC.Generics



data Config = Config{
    dataseedpath :: String,
    dbPath :: String
} deriving (Show,Generic)

instance FromJSON Config
instance ToJSON Config

configFile = "./config.json"
getJSON :: IO B.ByteString
getJSON = B.readFile configFile


readConfig :: IO (Maybe Config)
readConfig = do
    result <- (eitherDecode <$> getJSON) :: IO (Either String Config)
    case result of
        Right conf ->
            return (Just conf)
        Left err -> do
            putStrLn err
            return Nothing

getConfig = extract readConfig

getDataSeedPath ::IO (String)
getDataSeedPath = do
    dataseedpath <$> getConfig

getDbPath ::IO (String)
getDbPath = do
    dbPath <$> getConfig

extract :: IO (Maybe a) -> IO a
extract = (>>= maybe (ioError $ userError "Could not read config!") return)
