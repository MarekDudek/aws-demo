module ServantLibSpec where

import ServantLib

import Test.Hspec

spec :: Spec
spec = do 
  spec1

spec1 :: Spec
spec1 = 
  describe "api" $ do
    it "works" $ do
      pending
