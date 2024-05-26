{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Aeson qualified as Aeson
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Serialize qualified as Cereal

import Vary (Vary)

type TestVariant = Vary '[Int, Double, Char, String]

main :: IO ()
main = hspec $ do
  describe "Serialization with aeson" $ do
    prop "encodes properly" $ \(v :: TestVariant) -> do
      Aeson.encode v `shouldSatisfy` (not . LBS.null)
    prop "encode - decode roundtrips" $ \(v :: TestVariant) -> do
      Aeson.eitherDecode (Aeson.encode v) `shouldBe` Right v

  describe "Serialization with binary" $ do
    prop "encodes properly" $ \(v :: TestVariant) -> do
      Binary.encode v `shouldSatisfy` (not . LBS.null)
    prop "encode - decode roundtrips" $ \(v :: TestVariant) -> do
      Binary.decode (Binary.encode v) `shouldBe` v

  describe "Serialization with cereal" $ do
    prop "encodes properly" $ \(v :: TestVariant) -> do
      Cereal.encode v `shouldSatisfy` (not . BS.null)
    prop "encode - decode roundtrips" $ \(v :: TestVariant) -> do
      Cereal.decode (Cereal.encode v) `shouldBe` Right v
