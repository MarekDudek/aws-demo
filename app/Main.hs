module Main where

import Web.Scotty
import Database.PostgreSQL.Simple

import DbLib (awsDemoConnection, testConnectionByAddition)
import WebLib (startWebServer)


main :: IO ()
main = do
  i <- testConnectionByAddition
  print i

