module Language.Pbasm.Template.StateSpec where

import Test.Hspec

import Language.Pbasm.Template.State

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
