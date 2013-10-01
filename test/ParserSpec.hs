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
    it "parses a lowercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse address "" "abc"
      result `shouldBe` Address 0xABC

    it "parses an uppercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse address "" "ABC"
      result `shouldBe` Address 0xABC

    it "fails with less than 3 digits" $ do
      let result = show $ fromLeft $ parse address "" "ff"
      result `shouldContain` "expecting hexadecimal digit"

    -- it "fails with more than 3 digits" $ do
    --   let result = show $ fromLeft $ parse address "" "ffff"
    --   result `shouldContain` "expecting hexadecimal digit"

  describe "constant" $ do
    it "parses a lowercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse constant "" "ab"
      result `shouldBe` Constant 0xAB

    it "parses an uppercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse constant "" "AB"
      result `shouldBe` Constant 0xAB

    it "fails with less than 2 digits" $ do
      let result = show $ fromLeft $ parse constant "" "f"
      result `shouldContain` "expecting hexadecimal digit"

    -- it "fails with more than 2 digits" $ do
    --   let result = show $ fromLeft $ parse constant "" "fff"
    --   result `shouldContain` "expecting hexadecimal digit"

  describe "register" $ do
    it "parses a lowercase register name" $ do
      let result = fromRight $ parse register "" "s0"
      result `shouldBe` Register0

    it "parses an uppercase register name" $ do
      let result = fromRight $ parse register "" "S0"
      result `shouldBe` Register0

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse register "" "sz"
      result `shouldContain` "expecting hexadecimal digit"
