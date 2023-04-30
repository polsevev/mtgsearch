{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module DataFetcher
    ( fetchData
    ) where
import Network.HTTP.Client     
import Network.HTTP.Client.TLS      
import qualified Data.ByteString.Lazy as B
import Config

fetchData = do
    link <- getBulkDataLink

    manager <- newManager tlsManagerSettings

    request <- parseRequest link

    r <- httpLbs request manager

    return $ responseBody r