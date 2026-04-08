-- |
-- Module      : Network.JsonRpc.Server
-- Stability   : stable
-- Portability : GHC2021
--
-- A pure, declarative JSON-RPC 2.0 server. Build an immutable dispatch
-- table from a list of method definitions, then handle messages.
-- Handlers are effect-polymorphic via 'Control.Monad.Catch.MonadCatch'
-- from the @exceptions@ package, so they work in @IO@, @ReaderT@,
-- @ExceptT@, or any custom monad stack.
--
-- @
-- import Network.JsonRpc.Types
-- import Network.JsonRpc.Server
--
-- myServer :: MonadCatch m => Server m
-- myServer = mkServer
--   [ method \"add\" ((,) \<$\> param \"a\" \<*\> param \"b\") $ \\(a, b) ->
--       pure (Right (a + b :: Int))
--   , method_ \"ping\" (pure (Right (\"pong\" :: Text)))
--   ]
--
-- -- In your transport loop:
-- result <- handleRaw myServer defaultServerConfig inputBytes
-- @
module Network.JsonRpc.Server
  ( -- * Server construction (pure, declarative)
    Server
  , mkServer
  , MethodDef
  , Handler (..)
  , method
  , method_
  , rawMethod

    -- * Configuration
  , ServerConfig (..)
  , defaultServerConfig

    -- * Applicative parameter parser
  , ParamParser
  , param
  , optParam
  , fromJsonParams
  , withNamedParams
  , withPositionalParams
  , ParamError (..)

    -- * Dispatch
  , handleMessage
  , handleBatch
  , handleRaw

    -- * Pure test harness
  , runHandlerPure

    -- * Mutable server (opt-in extension)
  , MutableServer
  , newMutableServer
  , register
  , toServer
  , ReservedMethodError (..)
  ) where

import Network.JsonRpc.Server.Internal
import Network.JsonRpc.Server.ParamParser
  ( ParamError (..)
  , ParamParser
  , fromJsonParams
  , optParam
  , param
  , withNamedParams
  , withPositionalParams
  )
