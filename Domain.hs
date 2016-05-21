{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Control.Applicative

{-
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
-}

data Rsvp = Rsvp Text Text Text Text Text Text Text-- name guest
     deriving (Show)
