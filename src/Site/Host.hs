{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Site.Host
    ( host
    ) where
import Web.Scotty

import Algorithm.Search
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text.Lazy (pack)
import Database.SQLite.Simple (query)
import Algorithm.Lex (lexx)

host = scotty 3000 $ do
  post "/api/req" $ do

            query <-  param "query"
            --liftIO (putStrLn $ "\nOutput! " ++ show (lexx query))
            --cards <- liftIO (search query)
            result <- liftIO (search query)
            html $ mconcat ["<h1>", pack result, "</h1>"]

  get "/" $ file "src/Site/Static/index.html"


