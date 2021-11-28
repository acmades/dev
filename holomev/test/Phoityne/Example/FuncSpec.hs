module Phoityne.Example.FuncSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Go go go" $ do
    it "Start!" $ do
      True `shouldBe` True
  