module Language.Pbasm.Parser.StateSpec where

import Data.Map (empty, fromList)
import Test.Hspec

import Language.Pbasm.Core
import Language.Pbasm.Parser.State

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parserState" $ do
    it "parses returns an empty parser state" $ do
      parserState
        `shouldBe`
        State { stateAddress     = 0
              , stateLabelMap    = empty
              , stateConstantMap = empty }

  describe "addConstant" $ do
    it "adds a constant to the constant map" $ do
      addConstant (ConstantDirective "foo" 0) parserState
        `shouldBe`
        parserState {stateConstantMap = fromList [("foo", 0)]}

  describe "addLabel" $ do
    it "adds a label to the label map" $ do
      addLabel "foo" parserState
        `shouldBe`
        parserState {stateLabelMap = fromList [("foo", 0)]}

  describe "incrementAddress" $ do
    it "increments the address" $ do
      incrementAddress parserState
        `shouldBe`
        parserState {stateAddress = 1}
