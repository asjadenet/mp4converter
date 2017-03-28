module LearningLibSpec (main, spec) where

import           LearningLib
-- import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "last4" $ do
    it "returns last 4 caracters" $ do
       last4 "abcd.efg" `shouldBe` ".efg"
       last4 "abcd.mp3" `shouldBe` ".mp3"
  describe "cutFirstPart" $ do
    it "cuts first part of the string" $ do
       cutFirstPart "abcd.efg" `shouldBe` "efg"
       cutFirstPart "abcd.mp3" `shouldBe` "mp3"
       cutFirstPart "abcd.efg.mp3" `shouldBe` "efg.mp3"
  describe "wordsWhen" $ do
    it "wordsWhen splits string into list" $ do
      wordsWhen (=='=') "abc=cde" `shouldBe` ["abc", "cde"]
      wordsWhen (==',') "abc,cde,hijkl" `shouldBe` ["abc", "cde", "hijkl"]
  describe "mytake" $ do
    it "mytake katsetus case when süntaksi õppimiseks" $ do
      mytake 0 0 `shouldBe` 0
      mytake 0 1 `shouldBe` 0
      mytake 4 1 `shouldBe` 1
      mytake 4 2 `shouldBe` 2
  -- http://zvon.org/other/haskell/Outputprelude/break_f.html
  describe "How break works" $ do
    it "creates a tuple of two lists from the original one separated at condition boundary" $ do
      break (3==) [1,2,3,4,5] `shouldBe` ([1,2],[3,4,5])
      break (4==) [1,2,3,4,5] `shouldBe` ([1,2,3],[4,5])
      break (1==) [1,2,3,4,5] `shouldBe` ([],[1,2,3,4,5])
      break ('w'==) "hello - world" `shouldBe` ("hello - ","world")

  describe "getExtensionHelper" $ do
    it "returns first part" $ do
       getExtensionHelper 0 "abcd.efg.hij" `shouldBe` "abcd."
       getExtensionHelper 0 "4pm.efg.hij" `shouldBe` "4pm."

  describe "getFnameHelper" $ do
    it "returns last part reversed" $ do
       getFnameHelper 0 "abcd.efg.hij1" `shouldBe` "1jih.gfe"
       getFnameHelper 0 "m.efg.hij" `shouldBe` "jih.gfe"
       getFnameHelper 1 "abcd.efg.hij1" `shouldBe` "1jih.gfe"
       getFnameHelper 1 "m.efg.hij" `shouldBe` "jih.gfe"
       getFnameHelper 2 "abcd.efg.hij1" `shouldBe` "1jih.gfe"
       getFnameHelper 2 "m.efg.hij" `shouldBe` "jih"
       getFnameHelper 3 "abcd.efg.hij1" `shouldBe` "1jih.gfe"
       getFnameHelper 3 "m.efg.hij" `shouldBe` "jih"
