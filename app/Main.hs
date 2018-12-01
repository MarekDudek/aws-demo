module Main where

import Web.Scotty
import Database.PostgreSQL.Simple

import Control.Monad

import DbLib 
import WebLib 
import IOLib


main :: IO ()
main = do

  success <- testConnection
  putStrLn $ "connection OK ? " ++ (show success)

  conn <- awsDemoConnection

  printOneTwoThree
  printAgain

  presents <- allPresents conn
  printPresents presents

  children <- allChildren conn
  printChildren children
  return ()


