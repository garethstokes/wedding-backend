{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- scotty
import Web.Scotty as S

-- wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

-- database
import           Data.Text
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

-- random
import Network.HTTP.Types (status302)
import System.Environment (lookupEnv)
import Control.Monad.Logger    (runStderrLoggingT)
import Data.Text.Lazy (toStrict)

-- models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RsvpEntry
    name Text
    guest Text
    email Text
    attending Bool
    bus Bool
    dietry Text 
    message Text
    deriving Show
|]

-- db connection string
connStr = "host=localhost dbname=wedding user=wedding password=hanshotfirst port=5432"

-- db helpers
--runDB pool q = liftIO $ runSqlPersistMPool q pool

db query = runStderrLoggingT $ 
        withPostgresqlPool connStr 10 $ 
        \pool -> liftIO $ runSqlPersistMPool query pool

doMigrations = runMigration migrateAll

--inHandlerDb = liftIO . db
--inAppDb = liftIO . db

-- entry point
main :: IO ()
main = do

   -- migrate database before starting webserver
   db doMigrations

   -- grab the webserver port if one is passed in
   port <- lookupSetting "PORT" 3000

   -- start the webserver
   scotty port $ do
         
      liftIO $ db $ doMigrations --insert $ rsvp

      -- serve static files
      middleware $ staticPolicy (noDots >-> addBase "dist/")

      -- log requests to stdout
      middleware logStdoutDev

      -- ROUTES

      -- welcome homepage
      S.get "/" $ file "dist/index.html"

      -- rsvp :: post
      S.post "/rsvp" $ do

         -- pull params from body
         name           <- (param "name")          `rescue` (\msg -> return msg)
         guestName      <- (param "guestName")     `rescue` (\msg -> return msg)
         email          <- (param "email")         `rescue` (\msg -> return msg)
         dietry         <- (param "dietry")        `rescue` (\msg -> return msg)
         message        <- (param "message")       `rescue` (\msg -> return msg)
         attendStatus   <- (param "attend-status") `rescue` (\msg -> return msg)
         busStatus      <- (param "bus-status")    `rescue` (\msg -> return msg)

         -- create a domain model
         let rsvp = RsvpEntry (toStrict name) (toStrict guestName) (toStrict email) True True (toStrict dietry) (toStrict message)

         -- save to database

         --db $ insert $ RsvpEntry name guestName email attendStatus busStatus dietry message
         
         -- redirect to thank-you
         redirectTo "/#/thank-you"

-- helpers
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = maybe def read <$> lookupEnv env

-- web helpers
redirectTo location = do
   status status302
   setHeader "Location" location

