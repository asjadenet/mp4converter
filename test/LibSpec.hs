module LibSpec (main, spec) where

import           LearningLib
import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addPathToFileName" $
    it "adds path to filename" $ do
       addPathToFileName "c:/mp3" "abcd.efg.hij" `shouldBe` "c:/mp3/abcd.efg.hij"
       addPathToFileName "c:/mp4" "abcd.efg.hij" `shouldBe` "c:/mp4/abcd.efg.hij"

  describe "getExtension" $
    it "returns extension no matter how long" $ do
       getExtension "abcd.efg" `shouldBe` ".efg"
       getExtension "abcd.mp3" `shouldBe` ".mp3"
       getExtension "abcd.longext" `shouldBe` ".longext"
       getExtension "123.abcd.mp3" `shouldBe` ".mp3"

  describe "getFnameWithoutExt" $
    it "returns filename without extension" $ do
       getFnameWithoutExt "abcd.efg.ef.g.h" `shouldBe` "abcd.efg.ef.g"
       getFnameWithoutExt "abcd.efg.ef.g" `shouldBe` "abcd.efg.ef"

  describe "getM4a" $
    it "returns m4a filename" $ do
       getM4a "abcd.efg.ef.g.h" `shouldBe` "abcd.efg.ef.g.m4a"
       getM4a "abcd.efg.ef.g" `shouldBe` "abcd.efg.ef.m4a"
  describe "getMp3" $
    it "returns .mp3 file" $
      getMp3 "abc.def.ghi" `shouldBe` "abc.def.mp3"
  describe "getTxt" $
    it "returns .txt file" $
      getTxt "abc.def.ghi" `shouldBe` "abc.def.txt"

  describe "getMp3FileWithPath" $
    it "returns mp3 filename with path" $ do
       getMp3FileWithPath "c:/mp3" "abcd.efg.ef.g.h" `shouldBe` "c:/mp3/abcd.efg.ef.g.mp3"
       getMp3FileWithPath "c:/otherpath" "abcd.efg.ef.g" `shouldBe` "c:/otherpath/abcd.efg.ef.mp3"

  describe "getMetadataLines" $
    it "reads file lines" $
      length <$> getMetadataLines "C:/Src/haskell/mp4converter/test/metadata.txt" >>= (`shouldBe` 9)

  describe "getTitleLineFromMetaDataFile" $
    it "reads title line from metadata file" $ do
      getTitleLineFromMetaDataFile "C:/Src/haskell/mp4converter/test/metadata.txt" >>= (`shouldBe` "title=Test title")
      getTitleLineFromMetaDataFile "C:/Src/haskell/mp4converter/test/missing-title.txt" >>= (`shouldBe` "")

  describe "parseTitleLine" $
    it "parse title line" $ do
      parseTitleLine "title=Test title" `shouldBe` "Test title"
      parseTitleLine "" `shouldBe` ""

  describe "getTitleFromMetadataFile" $
    it "get title from metadata file" $ do
      getTitleFromMetadataFile "C:/Src/haskell/mp4converter/test/metadata.txt" >>= (`shouldBe` "Test title")
      getTitleFromMetadataFile "C:/Src/haskell/mp4converter/test/missing-title.txt" >>= (`shouldBe` "")

  describe "dropFullPath" $
    it "drops full path from filename" $
      dropFullPath "C:/Src/haskell/mp4converter/test/metadata.txt" `shouldBe` "metadata.txt"





