module DbLibSpec where

import DbLib

import Database.PostgreSQL.Simple
import Test.Hspec
import Control.Exception

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = before_ beforeEverySpecItem $ after_ afterEverySpecItem $ around_ aroundEverySpecItem $ do
  describe "Demo AWS connection" $
    it "can be established" $
      2 + 3 `shouldBe` 5
  describe "second" $
    it "can be established" $
      2 + 3 `shouldBe` 5


spec2 :: Spec
spec2 = before openConnection $ do
  describe "with connecton" $ do
    it "uses DB" $ \conn -> do
      2 + 3 `shouldBe` 5


openConnection :: IO Connection 
openConnection = awsDemoConnection 
      
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
        
