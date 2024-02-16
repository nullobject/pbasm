module Language.Pbasm.Parser.StateSpec where

import Data.Map (empty, fromList)
import Language.Pbasm.Core (Statement (..))
import Language.Pbasm.Parser.State
  ( State (..),
    addConstant,
    addLabel,
    incrementAddress,
    parserState,
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parserState" $ do
    it "parses returns an empty parser state" $ do
      parserState
        `shouldBe` State
          { stateAddress = 0,
            stateLabelMap = empty,
            stateConstantMap = empty
          }

  describe "addConstant" $ do
    it "adds a constant to the constant map" $ do
      addConstant (ConstantDirective "foo" 0) parserState
        `shouldBe` parserState {stateConstantMap = fromList [("foo", 0)]}

  describe "addLabel" $ do
    it "adds a label to the label map" $ do
      addLabel "foo" parserState
        `shouldBe` parserState {stateLabelMap = fromList [("foo", 0)]}

  describe "incrementAddress" $ do
    it "increments the address" $ do
      incrementAddress parserState
        `shouldBe` parserState {stateAddress = 1}
