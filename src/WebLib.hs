module WebLib where

import Web.Scotty

import Data.Monoid (mconcat)


startWebServer :: IO ()
startWebServer = scotty 8080 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
