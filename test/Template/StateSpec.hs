module Template.StateSpec where

import Template.State

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "templateState" $ do
    it "returns an empty template state" $ do
      templateState
        `shouldBe`
        State { stateName      = ""
              , stateOpcodes   = []
              , stateRendering = False
              , stateTimestamp = ""
              }
