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
  describe "address" $ do
    it "parses a 12-bit address" $ do
      let result = fromRight $ parse address "" "fff"
      result `shouldBe` Address 4095

    it "returns an error otherwise" $ do
      let result = show $ fromLeft $ parse address "" "ff"
      result `shouldContain` "expecting hexadecimal digit"

  describe "constant" $ do
    it "parses a 8-bit constant value" $ do
      let result = fromRight $ parse constant "" "ff"
      result `shouldBe` Constant 255

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

  describe "instructions" $ do
    it "parses a set of instructions" $ do
      let input = "LOAD s0, s1\n\
                  \LOAD s1, FF\n\
                  \AND s0, s1\n\
                  \AND s0, FF\n\
                  \OR s0, s1\n\
                  \OR s0, FF\n\
                  \XOR s0, s1\n\
                  \XOR s0, FF\n\
                  \CALL FFF\n\
                  \JUMP FFF\n\
                  \RETURN\n"

      let result = fromRight $ parse instructions "" input

      result `shouldBe` [
          LoadInstruction (RegisterOperand Register0) (RegisterOperand Register1)
        , LoadInstruction (RegisterOperand Register1) (ConstantOperand (Constant 255))
        , AndInstruction (RegisterOperand Register0) (RegisterOperand Register1)
        , AndInstruction (RegisterOperand Register0) (ConstantOperand (Constant 255))
        , OrInstruction (RegisterOperand Register0) (RegisterOperand Register1)
        , OrInstruction (RegisterOperand Register0) (ConstantOperand (Constant 255))
        , XorInstruction (RegisterOperand Register0) (RegisterOperand Register1)
        , XorInstruction (RegisterOperand Register0) (ConstantOperand (Constant 255))
        , CallInstruction (AddressOperand (Address 4095))
        , JumpInstruction (AddressOperand (Address 4095))
        , ReturnInstruction
        ]
