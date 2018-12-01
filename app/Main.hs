module Main where

import Web.Scotty
import Database.PostgreSQL.Simple

import Control.Monad

import DbLib 
import WebLib 
import IOLib


main :: IO ()
main = do
  conn <- awsDemoConnection
  printOneTwoThree
  printAgain
  presents <- allPresents conn
  putStrLn $ "presents: " ++ (show $ length presents)
  i <- testConnectionByAddition
  print $ "2 + 2 == " ++ (show i)

