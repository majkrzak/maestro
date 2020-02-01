module Majkrzak.Maestro.UtilsSpec (spec) where

import Test.Hspec
import Majkrzak.Maestro.Utils
import GHC.Generics (Generic)


data T1 = T1 deriving Generic
data T2 = T2a | T2b deriving Generic
data T3 = T3 Int String deriving Generic
data T4 = T4 { t4a::Int,  t4b::String } deriving Generic
data T5 = T5a | T5b Int String | T5c { t5a::Int,  t5b::String } deriving Generic

spec :: Spec
spec = describe "Majkrzak.Maestro.Utils name" $ do
  it "names unary types" $ do
    (name T1) `shouldBe` "T1"
  it "names simple sum types" $ do
    (name T2a) `shouldBe` "T2a"
    (name T2b) `shouldBe` "T2b"
  it "names parametrized types" $ do
    (name T3) `shouldBe` "T3"
  it "names record types" $ do
    (name T4) `shouldBe` "T4"
  it "names mixed sum types" $ do
    (name T5a) `shouldBe` "T5a"
    (name T5b) `shouldBe` "T5b"
    (name T5c) `shouldBe` "T5c"
