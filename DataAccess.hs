{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DataAccess where
import Domain

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Word (Word16)
import GHC.Int

-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String,
     dbHost :: String,
     dbPort :: Word16
     }
     deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       , connectHost = dbHost conf
                       , connectPort = dbPort conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

-- Update database
execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
      where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
       where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

insertRsvp :: Pool Connection -> Rsvp -> ActionT TL.Text IO ()
insertRsvp pool (Rsvp name guest email attending bus dietry message) = do
     liftIO $ execSqlT pool [name, guest, email, attending, bus, dietry, message]
                            "INSERT INTO rsvp(name, guest, email, attending, bus, dietry, message) VALUES(?,?,?,?,?,?,?)"
     return ()
