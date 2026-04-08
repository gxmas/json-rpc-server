{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.JsonRpc.Server.Internal
-- Stability   : unstable
-- Portability : GHC2021
--
-- __This is an internal module.__ The API may change between minor
-- versions without notice. Import "Network.JsonRpc.Server" for the
-- stable public interface.
--
-- Exports the internal representation of the server, dispatch logic,
-- and mutable server internals.
module Network.JsonRpc.Server.Internal
  ( -- * Server
    Server (..)
  , mkServer
  , Handler (..)
  , MethodDef (..)
  , method
  , method_
  , rawMethod

    -- * Configuration
  , ServerConfig (..)
  , defaultServerConfig

    -- * Dispatch
  , handleMessage
  , handleBatch
  , handleRaw
  , safeRunHandler

    -- * Pure test harness
  , runHandlerPure

    -- * Mutable server
  , MutableServer (..)
  , newMutableServer
  , register
  , toServer
  , ReservedMethodError (..)
  ) where

import Control.Monad.Catch (MonadCatch, SomeException, try)
import Data.Aeson (ToJSON, Value (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Network.JsonRpc.Codec (parse, serializeBatch, serializeResponse)
import Network.JsonRpc.Codec.Internal ()
import Network.JsonRpc.Server.ParamParser
import Network.JsonRpc.Types.Internal

-- ====================================================================
-- Handler
-- ====================================================================

-- | A method handler, polymorphic in its effect monad. The handler
-- receives the parsed 'Params' and returns either an 'ErrorObject'
-- (for application-level errors) or a JSON 'Value' (for success).
newtype Handler m = Handler
  { runHandler :: Params -> m (Either ErrorObject Value)
  }

-- ====================================================================
-- Server
-- ====================================================================

-- | A pure, immutable method dispatch table. Construct with 'mkServer'
-- from a list of 'MethodDef' values.
--
-- The server is a value, not an action. It can be defined at the top
-- level, shared across threads, and composed without IO.
newtype Server m = Server (HashMap Text (Handler m))

-- | A method definition: a name paired with a handler.
data MethodDef m = MethodDef !Text !(Handler m)

-- | Build a server from a list of method definitions. If a method name
-- appears more than once, the last definition wins.
mkServer :: [MethodDef m] -> Server m
mkServer defs = Server (HM.fromList [(n, h) | MethodDef n h <- defs])

-- ====================================================================
-- Method definition constructors
-- ====================================================================

-- | Define a method with applicative parameter parsing. The
-- 'ParamParser' extracts typed parameters with error aggregation,
-- and the handler function processes the parsed values.
--
-- @
-- method \"add\" ((,) \<$\> param \"a\" \<*\> param \"b\") $ \\(a, b) ->
--   pure (Right (toJSON (a + b :: Int)))
-- @
method
  :: (Monad m, ToJSON b)
  => Text
  -> ParamParser a
  -> (a -> m (Either ErrorObject b))
  -> MethodDef m
method name parser handler = MethodDef name $ Handler $ \params ->
  withNamedParams parser handler' params
  where
    handler' a = do
      result <- handler a
      pure (fmap Aeson.toJSON result)

-- | Define a method that takes no parameters. The handler is run
-- regardless of what parameters are sent.
--
-- @
-- method_ \"ping\" (pure (Right (toJSON (\"pong\" :: Text))))
-- @
method_
  :: (Monad m, ToJSON b)
  => Text
  -> m (Either ErrorObject b)
  -> MethodDef m
method_ name handler = MethodDef name $ Handler $ \_ -> do
  result <- handler
  pure (fmap Aeson.toJSON result)

-- | Define a method with raw 'Params' access. Use this when the
-- applicative parser does not fit your needs.
rawMethod :: Text -> (Params -> m (Either ErrorObject Value)) -> MethodDef m
rawMethod name handler = MethodDef name (Handler handler)

-- ====================================================================
-- Configuration
-- ====================================================================

-- | Server configuration. The 'onError' callback requires the handler
-- monad to be 'IO'-compatible. For pure testing contexts where the
-- callback is not needed, use 'defaultServerConfig'.
data ServerConfig = ServerConfig
  { -- | Called when a handler returns an error or throws an exception.
    -- Arguments: method name, error object. This callback is only
    -- invoked for notification handlers (where the error cannot be
    -- returned to the caller).
    onError :: !(Text -> ErrorObject -> IO ())
  }

-- | Default server configuration with no-op callbacks.
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { onError = \_ _ -> pure ()
    }

-- ====================================================================
-- Dispatch
-- ====================================================================

-- | Look up a handler by method name.
lookupHandler :: Server m -> Text -> Maybe (Handler m)
lookupHandler (Server table) name = HM.lookup name table

-- | Run a handler, catching any exceptions and converting them to
-- internal errors. This ensures a misbehaving handler never crashes
-- the server.
safeRunHandler :: MonadCatch m => Handler m -> Params -> m (Either ErrorObject Value)
safeRunHandler h params = do
  result <- try (runHandler h params)
  case result of
    Left (e :: SomeException) ->
      pure (Left (internalError (Just (String (T.pack (show e))))))
    Right val -> pure val

-- | Handle a single parsed 'Message'.
--
-- * 'MsgRequest' -- dispatches to the matching handler and returns
--   @'Just' response@.
-- * 'MsgNotification' -- dispatches to the matching handler (fire and
--   forget) and returns 'Nothing'. Handler errors are silently
--   discarded in the return value (the spec forbids responding to
--   notifications).
-- * 'MsgResponse' -- ignored (a server should not receive responses).
--   Returns 'Nothing'.
handleMessage :: MonadCatch m => Server m -> ServerConfig -> Message -> m (Maybe Response)
handleMessage server _config (MsgRequest req) = do
  let mid = requestId req
  case lookupHandler server (requestMethod req) of
    Nothing -> pure (Just (ResponseFailure (Failure (methodNotFound Nothing) mid)))
    Just h -> do
      result <- safeRunHandler h (requestParams req)
      case result of
        Left err -> pure (Just (ResponseFailure (Failure err mid)))
        Right val -> pure (Just (ResponseSuccess (Success val mid)))
handleMessage server _config (MsgNotification notif) = do
  case lookupHandler server (notificationMethod notif) of
    Nothing -> pure Nothing
    Just h -> do
      -- Fire and forget. The spec forbids responding to notifications.
      -- Errors are discarded; use handleRaw (which runs in IO) if you
      -- need the onError callback.
      _ <- safeRunHandler h (notificationParams notif)
      pure Nothing
handleMessage _ _ (MsgResponse _) = pure Nothing

-- | Handle a batch of parsed elements. Returns 'Nothing' if all
-- elements are notifications (no responses to send).
--
-- Invalid elements produce error responses per the spec; they are
-- not silently dropped.
handleBatch :: MonadCatch m => Server m -> ServerConfig -> [BatchElement] -> m (Maybe [Response])
handleBatch server config elems = do
  results <- mapM handleElement elems
  let responses = [r | Just r <- results]
  pure $ if null responses then Nothing else Just responses
  where
    handleElement (ValidElement msg) = handleMessage server config msg
    handleElement (InvalidElement failure) = pure (Just (ResponseFailure failure))

-- | Handle raw JSON bytes end-to-end: parse, dispatch, serialize.
--
-- This is the primary entry point for most server implementations.
-- It takes raw bytes from the transport and returns raw bytes to
-- send back (or 'Nothing' for all-notification batches).
--
-- @
-- transport <- getTransport
-- bytes <- receive transport
-- result <- handleRaw server defaultServerConfig bytes
-- case result of
--   Just response -> send transport response
--   Nothing       -> pure ()
-- @
handleRaw :: MonadCatch m => Server m -> ServerConfig -> ByteString -> m (Maybe ByteString)
handleRaw server config bs = case parse bs of
  ParseFailed err ->
    pure (Just (serializeResponse (ResponseFailure (Failure err Nothing))))
  SingleMessage msg -> do
    result <- handleMessage server config msg
    pure (fmap serializeResponse result)
  BatchMessage elems -> do
    result <- handleBatch server config elems
    pure (fmap (serializeBatch . map MsgResponse) result)

-- ====================================================================
-- Pure test harness
-- ====================================================================

-- | Run a handler in a pure context for testing. Because handlers are
-- effect-polymorphic, they can be instantiated at 'Identity'.
--
-- @
-- let result = runHandlerPure myHandler (ParamsByName params)
-- result \`shouldBe\` Right (toJSON expected)
-- @
runHandlerPure :: Handler Identity -> Params -> Either ErrorObject Value
runHandlerPure h params = runIdentity (runHandler h params)

-- ====================================================================
-- Mutable server
-- ====================================================================

-- | A mutable server that supports dynamic handler registration.
-- This is an opt-in extension for use cases like hot-reloading or
-- plugin systems. For most applications, the pure 'Server' is
-- preferred.
data MutableServer m = MutableServer
  { mutableHandlers :: !(IORef (HashMap Text (Handler m)))
  , mutableConfig :: !ServerConfig
  }

-- | Indicates that a method name starting with @\"rpc.\"@ was
-- rejected. The spec reserves this prefix for system extensions.
data ReservedMethodError = ReservedMethodError !Text
  deriving (Eq, Show)

-- | Create a new empty mutable server.
newMutableServer :: ServerConfig -> IO (MutableServer m)
newMutableServer config = do
  ref <- newIORef HM.empty
  pure (MutableServer ref config)

-- | Register a handler for a method name. Returns @'Left'
-- 'ReservedMethodError'@ if the name starts with @\"rpc.\"@
-- (reserved by the spec). If a handler for this name already exists,
-- it is replaced.
register :: MutableServer m -> Text -> Handler m -> IO (Either ReservedMethodError ())
register ms name handler
  | "rpc." `T.isPrefixOf` name = pure (Left (ReservedMethodError name))
  | otherwise = do
      atomicModifyIORef' (mutableHandlers ms) $ \hm ->
        (HM.insert name handler hm, ())
      pure (Right ())

-- | Snapshot the current mutable registry into an immutable 'Server'.
-- The returned server is a frozen copy; subsequent registrations on
-- the 'MutableServer' do not affect it.
toServer :: MutableServer m -> IO (Server m)
toServer ms = Server <$> readIORef (mutableHandlers ms)
