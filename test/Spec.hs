-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- spec-discover millegipärast ei tööta, seadistame käsitsi

import Test.Hspec

import qualified LibSpec
import qualified LearningLibSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lib"             LibSpec.spec
  describe "LearningLib"     LearningLibSpec.spec
