{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do

  -- Define your routes and handlers here
  get "/" $ do
    text "Hello, Haskell Web Service!"