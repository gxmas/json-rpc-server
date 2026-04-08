{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.ServerSpec (spec) where

import Control.Exception (throwIO)
import Data.Aeson (Value (..), toJSON)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor.Identity (Identity (..))
import Network.JsonRpc.Codec
import Network.JsonRpc.Codec.Internal ()
import Network.JsonRpc.Server
import Network.JsonRpc.Types
import Network.JsonRpc.Types.Internal (ErrorObject (..))
import Test.Hspec

-- | A simple test server with an "add" method and a "ping" method.
testServer :: Server IO
testServer =
  mkServer
    [ rawMethod "add" $ \params ->
        case params of
          ParamsByName _ -> pure (Right (toJSON (42 :: Int)))
          _ -> pure (Left (invalidParams Nothing))
    , method_ "ping" (pure (Right ("pong" :: String)))
    , rawMethod "crash" $ \_ -> do
        _ <- throwIO (userError "handler exploded")
        pure (Right Null)
    ]

spec :: Spec
spec = do
  describe "Server construction" $ do
    it "mkServer builds a server from method defs" $ do
      let s = mkServer [method_ "test" (pure (Right (42 :: Int)))] :: Server IO
      result <- handleMessage s defaultServerConfig (MsgRequest (Request "test" NoParams (Just (IdNum 1))))
      case result of
        Just (ResponseSuccess _) -> pure ()
        other -> expectationFailure ("expected ResponseSuccess, got: " ++ show other)

  describe "Handler dispatch" $ do
    it "dispatches to the correct handler" $ do
      result <- handleMessage testServer defaultServerConfig
        (MsgRequest (Request "ping" NoParams (Just (IdNum 1))))
      case result of
        Just (ResponseSuccess s) -> do
          successResult s `shouldBe` toJSON ("pong" :: String)
          successId s `shouldBe` Just (IdNum 1)
        other -> expectationFailure ("expected ResponseSuccess, got: " ++ show other)

  describe "Method not found" $ do
    it "returns -32601 for unknown methods" $ do
      result <- handleMessage testServer defaultServerConfig
        (MsgRequest (Request "nonexistent" NoParams (Just (IdNum 1))))
      case result of
        Just (ResponseFailure f) ->
          errorCode (failureError f) `shouldBe` (-32601)
        other -> expectationFailure ("expected ResponseFailure, got: " ++ show other)

  describe "Notification suppression" $ do
    it "returns Nothing for notifications" $ do
      result <- handleMessage testServer defaultServerConfig
        (MsgNotification (Notification "ping" NoParams))
      result `shouldBe` Nothing

  describe "Handler exception safety" $ do
    it "catches handler exceptions and returns InternalError" $ do
      result <- handleMessage testServer defaultServerConfig
        (MsgRequest (Request "crash" NoParams (Just (IdNum 1))))
      case result of
        Just (ResponseFailure f) ->
          errorCode (failureError f) `shouldBe` (-32603)
        other -> expectationFailure ("expected InternalError, got: " ++ show other)

  describe "handleRaw" $ do
    it "round-trips raw JSON" $ do
      let input = LBS.pack "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1}"
      result <- handleRaw testServer defaultServerConfig input
      case result of
        Just bs -> case parse bs of
          SingleMessage (MsgResponse (ResponseSuccess _)) -> pure ()
          other -> expectationFailure ("expected success response, got: " ++ show other)
        Nothing -> expectationFailure "expected Just response"

    it "returns ParseFailed for invalid JSON" $ do
      result <- handleRaw testServer defaultServerConfig "not json"
      case result of
        Just bs -> case parse bs of
          SingleMessage (MsgResponse (ResponseFailure f)) ->
            errorCode (failureError f) `shouldBe` (-32700)
          other -> expectationFailure ("expected parse error response, got: " ++ show other)
        Nothing -> expectationFailure "expected Just response"

  describe "Batch processing" $ do
    it "handles a mixed batch" $ do
      let input =
            LBS.pack $
              concat
                [ "["
                , "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1},"
                , "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"}"
                , "]"
                ]
      result <- handleRaw testServer defaultServerConfig input
      case result of
        Just bs -> case parse bs of
          SingleMessage (MsgResponse _) -> pure () -- single response for the one request
          BatchMessage _ -> pure ()
          other -> expectationFailure ("expected response(s), got: " ++ show other)
        Nothing -> expectationFailure "expected Just response"

    it "returns Nothing for all-notification batch" $ do
      let input =
            LBS.pack $
              concat
                [ "["
                , "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"},"
                , "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"}"
                , "]"
                ]
      result <- handleRaw testServer defaultServerConfig input
      result `shouldBe` Nothing

  describe "Pure test harness" $ do
    it "runHandlerPure works with Identity monad" $ do
      let h = Handler $ \_ -> Identity (Right (toJSON (42 :: Int)))
      runHandlerPure h NoParams `shouldBe` Right (toJSON (42 :: Int))

  describe "MutableServer" $ do
    it "rejects rpc. prefix" $ do
      ms <- newMutableServer defaultServerConfig :: IO (MutableServer IO)
      result <- register ms "rpc.foo" (Handler $ \_ -> pure (Right Null))
      result `shouldBe` Left (ReservedMethodError "rpc.foo")

    it "accepts normal method names" $ do
      ms <- newMutableServer defaultServerConfig :: IO (MutableServer IO)
      result <- register ms "foo" (Handler $ \_ -> pure (Right Null))
      result `shouldBe` Right ()

    it "toServer snapshots the registry" $ do
      ms <- newMutableServer defaultServerConfig :: IO (MutableServer IO)
      _ <- register ms "foo" (Handler $ \_ -> pure (Right (toJSON (1 :: Int))))
      s <- toServer ms
      result <- handleMessage s defaultServerConfig
        (MsgRequest (Request "foo" NoParams (Just (IdNum 1))))
      case result of
        Just (ResponseSuccess _) -> pure ()
        other -> expectationFailure ("expected success, got: " ++ show other)
