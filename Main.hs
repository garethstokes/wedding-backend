{-# LANGUAGE OverloadedStrings          #-}

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status302)
import System.Environment (lookupEnv)

import DataAccess
import Domain

-- entry point
main :: IO ()
main = do

   -- grab the webserver port if one is passed in
   port <- lookupSetting "PORT" 3000
   db_name <- lookupSetting "DB_NAME" "wedding"
   db_user <- lookupSetting "DB_USER" "garethstokes"
   db_pass <- lookupSetting "DB_PASS" ""
   db_host <- lookupSetting "DB_HOST" "."
   db_port <- lookupSetting "DB_PORT" 5342

   let dbConfig = DbConfig db_name db_user db_pass db_host db_port

   -- db init
   pool <- createPool (newConn dbConfig) close 1 40 10

   -- start the webserver
   scotty port $ do
         
      -- serve static files
      middleware $ staticPolicy (noDots >-> addBase "dist/")

      -- log requests to stdout
      middleware logStdoutDev

      -- ROUTES

      -- welcome homepage
      get "/" $ file "dist/index.html"

      -- rsvp :: post
      post "/rsvp" $ do

         -- pull params from body
         name           <- (param "name")          `rescue` (\msg -> return msg)
         guestName      <- (param "guest-name")     `rescue` (\msg -> return msg)
         email          <- (param "email")         `rescue` (\msg -> return msg)
         dietry         <- (param "dietry")        `rescue` (\msg -> return msg)
         message        <- (param "message")       `rescue` (\msg -> return msg)
         attendStatus   <- (param "attend-status") `rescue` (\msg -> return msg)
         busStatus      <- (param "bus-status")    `rescue` (\msg -> return msg)

         -- create a domain model
         --let rsvp = RsvpEntry (toStrict name) (toStrict guestName) (toStrict email) True True (toStrict dietry) (toStrict message)
         let rsvp = Rsvp name guestName email attendStatus busStatus dietry message

         -- save to database
         insertRsvp pool rsvp

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

