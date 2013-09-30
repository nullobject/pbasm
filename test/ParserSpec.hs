module ParserSpec where

import Core
import Parser

import Test.Hspec
import Text.ParserCombinators.Parsec

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "no right value"

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "no left value"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "address" $ do
    it "parses a 12-bit address" $ do
      let result = fromRight $ parse address "" "fff"
      result `shouldBe` Address 0xFFF

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse address "" "ff"
      result `shouldContain` "expecting hexadecimal digit"

  describe "constant" $ do
    it "parses a 8-bit constant value" $ do
      let result = fromRight $ parse constant "" "ff"
      result `shouldBe` Constant 0xFF

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse constant "" "f"
      result `shouldContain` "expecting hexadecimal digit"

  describe "register" $ do
    it "parses a register name" $ do
      let result = fromRight $ parse register "" "s0"
      result `shouldBe` Register0

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse register "" "sz"
      result `shouldContain` "expecting hexadecimal digit"
