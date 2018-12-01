{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Database.PostgreSQL.Simple
import Data.Monoid (mconcat)

import Lib
import DbLib (awsDemoConnection)


main :: IO ()
main = do
  i <- testConnectionByAddition
  print i

startWebServer :: IO ()
startWebServer = scotty 8080 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

connectionString = "postgresql://aws-demo-user:aws-demo-password@localhost:5432/aws-demo"

testConnectionByAddition :: IO Int
testConnectionByAddition = do
  conn <- connectPostgreSQL connectionString
  [Only i] <- query_ conn "select 2 + 2"
  return i

inMonadicStyle = do
  conn <- awsDemoConnection
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 3" :: IO [Only Int] )
  
