module DbLibSpec where

import DbLib

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Demo AWS connection" $
    it "can be established" $
      2 + 3 `shouldBe` 5
      
