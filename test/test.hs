import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "We have tests" $ do
    it "this should be this, that should be that" $ do
      "this" `shouldBe` "this"
      "that" `shouldBe` "that"
