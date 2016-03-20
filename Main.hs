{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import System.Environment (lookupEnv)

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = maybe def read <$> lookupEnv env

main :: IO ()
main = do
  port <- lookupSetting "PORT" 3000
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "dist/")
    get "/" $ file "dist/index.html"
