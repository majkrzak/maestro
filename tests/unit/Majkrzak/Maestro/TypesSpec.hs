module Majkrzak.Maestro.TypesSpec
  ( spec
  )
where

import Test.Hspec
import Majkrzak.Maestro.Types
import GHC.Generics (Generic)


data T1 = T1a deriving (Show, Eq, Generic, Event)
data T2 = T2a | T2b deriving (Show, Eq, Generic, Event)
data T3 = T3a Int String deriving (Show, Eq, Generic, Event)
data T4 = T4a { t4a1::Int,  t4a2::String } deriving (Show, Eq, Generic, Event)

spec :: Spec
spec = describe "Event load" $ do
  context "without payload" $ do
    let payload = ""
    it "loads event as single empty type" $ do
      (load "T1a" payload) `shouldBe` Just T1a
    it "loads event as sum of empty types" $ do
      (load "T2a" payload) `shouldBe` Just T2a
      (load "T2b" payload) `shouldBe` Just T2b
    it "fails to load event as single simple type" $ do
      (load "T3a" payload) `shouldBe` (Nothing :: Maybe T3)
    it "fails to load evemt as single record type" $ do
      (load "T4a" payload) `shouldBe` (Nothing :: Maybe T4)
  context "with array payload" $ do
    let payload = "[123,\"123\"]"
    it "loads event as single empty type" $ do
      (load "T1a" payload) `shouldBe` Just T1a
    it "loads event as sum of empty types" $ do
      (load "T2a" payload) `shouldBe` Just T2a
      (load "T2b" payload) `shouldBe` Just T2b
    it "loads event as single simple type" $ do
      (load "T3a" payload) `shouldBe` Just (T3a 123 "123")
    it "fails to load evemt as single record type" $ do
      (load "T4a" payload) `shouldBe` (Nothing :: Maybe T4)
  context "with object payload" $ do
    let payload = "{\"t4a1\":123,\"t4a2\":\"123\"}"
    it "loads event as single empty type" $ do
      (load "T1a" payload) `shouldBe` Just T1a
    it "loads event as sum of empty types" $ do
      (load "T2a" payload) `shouldBe` Just T2a
      (load "T2b" payload) `shouldBe` Just T2b
    it "loads event as single simple type" $ do
      (load "T3a" payload) `shouldBe` (Nothing :: Maybe T3)
    it "fails to load evemt as single record type" $ do
      (load "T4a" payload) `shouldBe` Just (T4a 123 "123")
  context "with invalid payload" $ do
    let payload = "INVALID"
    it "loads event as single empty type" $ do
      (load "T1a" payload) `shouldBe` Just T1a
    it "loads event as sum of empty types" $ do
      (load "T2a" payload) `shouldBe` Just T2a
      (load "T2b" payload) `shouldBe` Just T2b
    it "fails to load event as single simple type" $ do
      (load "T3a" payload) `shouldBe` (Nothing :: Maybe T3)
    it "fails to load evemt as single record type" $ do
      (load "T4a" payload) `shouldBe` (Nothing :: Maybe T4)
