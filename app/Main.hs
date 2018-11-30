{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Database.PostgreSQL.Simple
import Data.Monoid (mconcat)

import Lib

main :: IO ()
main = do
  i <- hello
  print i

startWebServer :: IO ()
startWebServer = scotty 8080 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

hello :: IO Int
hello = do
  conn <- connectPostgreSQL "postgresql://aws-demo-user:aws-demo-password@localhost:5432/aws-demo"
  [Only i] <- query_ conn "select 2 + 2"
  return i
