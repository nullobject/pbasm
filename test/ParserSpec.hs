module ParserSpec where

import Test.Hspec
import Text.ParserCombinators.Parsec

import Parser

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
  describe ".address" $ do
    it "parses a 12-bit address" $ do
      let result = fromRight $ parse address "" "fff"
      result `shouldBe` Address 4095

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse address "" "ff"
      result `shouldContain` "expecting hexadecimal digit"

  describe ".constant" $ do
    it "parses a 8-bit constant value" $ do
      let result = fromRight $ parse constant "" "ff"
      result `shouldBe` Constant 255

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse constant "" "f"
      result `shouldContain` "expecting hexadecimal digit"

  describe ".register" $ do
    it "parses a register name" $ do
      let result = fromRight $ parse register "" "s0"
      result `shouldBe` Register0

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse register "" "sz"
      result `shouldContain` "expecting hexadecimal digit"

  describe ".loadInstruction" $ do
    it "parses a 'LOAD sX, sY' instruction" $ do
      let result = fromRight $ parse loadInstruction "" "LOAD s0, s1"
      result `shouldBe` LoadRegisterInstruction Register0 Register1

    it "parses a 'LOAD sX, kk' instruction" $ do
      let result = fromRight $ parse loadInstruction "" "LOAD s0, ff"
      result `shouldBe` LoadConstantInstruction Register0 (Constant 255)

  describe ".andInstruction" $ do
    it "parses a 'AND sX, sY' instruction" $ do
      let result = fromRight $ parse andInstruction "" "AND s0, s1"
      result `shouldBe` AndRegisterInstruction Register0 Register1

    it "parses a 'AND sX, kk' instruction" $ do
      let result = fromRight $ parse andInstruction "" "AND s0, ff"
      result `shouldBe` AndConstantInstruction Register0 (Constant 255)

  describe ".callInstruction" $ do
    it "parses a 'CALL aaa' instruction" $ do
      let result = fromRight $ parse callInstruction "" "CALL fff"
      result `shouldBe` CallInstruction (Address 4095)

  describe ".jumpInstruction" $ do
    it "parses a 'JUMP aaa' instruction" $ do
      let result = fromRight $ parse jumpInstruction "" "JUMP fff"
      result `shouldBe` JumpInstruction (Address 4095)

  describe ".returnInstruction" $ do
    it "parses a 'RETURN' instruction" $ do
      let result = fromRight $ parse returnInstruction "" "RETURN"
      result `shouldBe` ReturnInstruction
