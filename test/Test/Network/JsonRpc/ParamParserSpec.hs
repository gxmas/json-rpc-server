{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.ParamParserSpec (spec) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Network.JsonRpc.Server.ParamParser
import Network.JsonRpc.Types
import Network.JsonRpc.Types.Internal (ErrorObject (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Single param extraction" $ do
    it "extracts a named param" $ do
      let p = ParamsByName (KM.singleton (Key.fromText "a") (Number 42))
      case runParamParser (param "a" :: ParamParser Int) p of
        VSuccess 42 -> pure ()
        other -> expectationFailure ("expected VSuccess 42, got: " ++ show other)

  describe "Multi-param applicative" $ do
    it "extracts multiple params" $ do
      let p =
            ParamsByName $
              KM.fromList
                [ (Key.fromText "a", Number 1)
                , (Key.fromText "b", Number 2)
                ]
      let parser = (,) <$> (param "a" :: ParamParser Int) <*> (param "b" :: ParamParser Int)
      case runParamParser parser p of
        VSuccess (1, 2) -> pure ()
        other -> expectationFailure ("expected (1, 2), got: " ++ show other)

  describe "Error aggregation" $ do
    it "collects all missing params" $ do
      let parser = (,) <$> (param "a" :: ParamParser Int) <*> (param "b" :: ParamParser Int)
      case runParamParser parser NoParams of
        VFailure errs -> length errs `shouldBe` 2
        other -> expectationFailure ("expected VFailure with 2 errors, got: " ++ show other)

  describe "Type mismatch" $ do
    it "reports type mismatch" $ do
      let p = ParamsByName (KM.singleton (Key.fromText "a") (String "not a number"))
      case runParamParser (param "a" :: ParamParser Int) p of
        VFailure [TypeMismatch "a" _] -> pure ()
        other -> expectationFailure ("expected TypeMismatch, got: " ++ show other)

  describe "Optional param" $ do
    it "returns default when absent" $ do
      case runParamParser (optParam "a" (42 :: Int)) NoParams of
        VSuccess 42 -> pure ()
        other -> expectationFailure ("expected 42, got: " ++ show other)

    it "returns value when present" $ do
      let p = ParamsByName (KM.singleton (Key.fromText "a") (Number 99))
      case runParamParser (optParam "a" (42 :: Int)) p of
        VSuccess 99 -> pure ()
        other -> expectationFailure ("expected 99, got: " ++ show other)

  describe "NoParams" $ do
    it "param on NoParams -> MissingParam" $ do
      case runParamParser (param "a" :: ParamParser Int) NoParams of
        VFailure [MissingParam "a"] -> pure ()
        other -> expectationFailure ("expected MissingParam, got: " ++ show other)

  describe "withNamedParams" $ do
    it "bridges parser to handler return type" $ do
      let parser = param "a" :: ParamParser Int
      let handler a = pure (Right (Number (fromIntegral a)))
      let p = ParamsByName (KM.singleton (Key.fromText "a") (Number 42))
      result <- withNamedParams parser handler p
      result `shouldBe` Right (Number 42)

    it "returns invalidParams on parse failure" $ do
      let parser = param "a" :: ParamParser Int
      let handler a = pure (Right (Number (fromIntegral a)))
      result <- withNamedParams parser handler NoParams
      case result of
        Left err -> errorCode err `shouldBe` (-32602)
        Right _ -> expectationFailure "expected Left"

  describe "withPositionalParams" $ do
    it "extracts positional params" $ do
      let p = ParamsByPosition (mempty)
      result <- withPositionalParams (\(_ :: [Int]) -> pure (Right (toJSON ([] :: [Int])))) p
      case result of
        Right _ -> pure ()
        Left e -> expectationFailure ("expected Right, got Left: " ++ show e)

  describe "fromJsonParams" $ do
    it "parses via FromJSON" $ do
      let p = ParamsByName (KM.singleton (Key.fromText "a") (Number 42))
      case runParamParser (fromJsonParams :: ParamParser (KM.KeyMap Value)) p of
        VSuccess _ -> pure ()
        other -> expectationFailure ("expected VSuccess, got: " ++ show other)
