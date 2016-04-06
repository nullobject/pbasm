module Language.Pbasm.TemplateSpec where

import Test.Hspec

import Language.Pbasm.Template

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runTemplate" $ do
    context "when the template is not rendering" $ do
      it "renders the lines after the 'begin template' tag" $ do
        let state = templateState
        runTemplate "tofu\n{begin template}paleo\n" state
          `shouldReturn`
          "paleo\n"

    context "when the template is rendering" $ do
      it "ignores empty tags" $ do
        let state = templateState {stateRendering = True}
        runTemplate "tofu\n{} paleo\n" state
          `shouldReturn`
          "tofu\n paleo\n"

      it "replaces the 'name' variable" $ do
        let state = templateState {stateRendering = True, stateName = "fixie"}
        runTemplate "tofu\n{name} paleo\n" state
          `shouldReturn`
          "tofu\nfixie paleo\n"

      it "replaces a 'timestamp' variable" $ do
        let state = templateState {stateRendering = True, stateTimestamp = "12345678"}
        runTemplate "tofu\n{timestamp} paleo\n" state
          `shouldReturn`
          "tofu\n12345678 paleo\n"

      it "replaces a 'INIT_kk' variable when the bank exists" $ do
        let state = templateState {stateRendering = True, stateOpcodes = [0xF0000..0xF0007]}
        runTemplate "tofu\n{INIT_00} paleo\n" state
          `shouldReturn`
          "tofu\n0000000000000000000000000000000000070006000500040003000200010000 paleo\n"

      it "replaces a 'INIT_kk' variable when the bank does not exist" $ do
        let state = templateState {stateRendering = True}
        runTemplate "tofu\n{INIT_01} paleo\n" state
          `shouldReturn`
          "tofu\n0000000000000000000000000000000000000000000000000000000000000000 paleo\n"

      it "replaces a 'INITP_kk' variable when the bank exists" $ do
        let state = templateState {stateRendering = True, stateOpcodes = [0x00000, 0x10000..0xF0007]}
        runTemplate "tofu\n{INITP_00} paleo\n" state
          `shouldReturn`
          "tofu\n00000000000000000000000000000000000000000000000000000000E4E4E4E4 paleo\n"

      it "replaces a 'INIT_kk' variable when the bank does not exist" $ do
        let state = templateState {stateRendering = True}
        runTemplate "tofu\n{INITP_01} paleo\n" state
          `shouldReturn`
          "tofu\n0000000000000000000000000000000000000000000000000000000000000000 paleo\n"
