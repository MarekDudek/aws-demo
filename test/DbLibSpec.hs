module DbLibSpec where

import DbLib

import Database.PostgreSQL.Simple
import Test.Hspec
import Control.Exception
import Data.Semigroup

spec :: Spec
spec = do 
  spec1 
  spec2

spec1 :: Spec
spec1 = before_ beforeEverySpecItem $ after_ afterEverySpecItem $ around_ aroundEverySpecItem $ do
  describe "Demo AWS connection" $
    it "can be established" $
      2 + 3 `shouldBe` 5
  describe "Second Demo" $
    it "can be established" $
      2 + 3 `shouldBe` 5

spec2 :: Spec
spec2 = before openConnection $ after closeConnection $ do
  describe "users" $ do
    it "has 3 elements" $ \conn -> do
      putStrLn "Counting users ..."
      count <- countUsersQuery conn
      count `shouldBe` 2
    it "has 3 elements" $ \conn -> do
      putStrLn "Counting users again ..."
      count <- countUsersQuery conn
      count `shouldBe` 2


openConnection :: IO Connection 
openConnection = do
  putStrLn "connecting..."
  awsDemoConnection 

closeConnection :: Connection -> IO () 
closeConnection c = do
  putStrLn "closing..."
  close c

      
beforeEverySpecItem :: IO ()
beforeEverySpecItem = do
  putStrLn "!!! Before every spec item !!!"


afterEverySpecItem :: IO ()
afterEverySpecItem = do
  putStrLn "!!! After every spec item !!!"

aroundEverySpecItem action = 
  bracket first last inBetween
  where first = (putStrLn "!!! First !!!")
        last _ = do
          putStrLn "!!! Last !!!"
          return 123
        inBetween _ = do
          putStrLn "!!! in-between !!!"
        
