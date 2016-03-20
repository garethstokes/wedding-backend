{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "dist/")
  get "/" $ file "dist/index.html"
