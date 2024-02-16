module Language.Pbasm.Template.StateSpec where

import Language.Pbasm.Template.State (State (..), templateState)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "templateState" $ do
    it "returns an empty template state" $ do
      templateState
        `shouldBe` State
          { stateName = "",
            stateOpcodes = [],
            stateRendering = False,
            stateTimestamp = ""
          }
