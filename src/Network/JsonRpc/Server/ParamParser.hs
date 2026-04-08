{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.JsonRpc.Server.ParamParser
-- Stability   : stable
-- Portability : GHC2021
--
-- Applicative parameter parser with error aggregation. Use 'param' and
-- 'optParam' to extract typed values from named parameters, then
-- combine with @('<*>')@. All errors are collected before failing,
-- giving API consumers better diagnostics than short-circuiting parsers.
--
-- @
-- addParser :: ParamParser (Int, Int)
-- addParser = (,) \<$\> param \"a\" \<*\> param \"b\"
-- @
module Network.JsonRpc.Server.ParamParser
  ( -- * Parser type
    ParamParser (..)

    -- * Combinators
  , param
  , optParam
  , fromJsonParams

    -- * Running
  , withNamedParams
  , withPositionalParams

    -- * Error types
  , ParamError (..)

    -- * Internal: Validation type
  , Validation (..)
  , validationToEither
  ) where

import Data.Aeson (FromJSON, Value (..), fromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.JsonRpc.Types.Internal

-- ====================================================================
-- Validation
-- ====================================================================

-- | Applicative functor that accumulates errors in a list, unlike
-- 'Either' which short-circuits. This is a local definition to avoid
-- a dependency on the @validation@ package for a trivial type.
data Validation e a
  = VFailure !e
  | VSuccess !a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (VFailure e) = VFailure e
  fmap f (VSuccess a) = VSuccess (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = VSuccess
  VFailure e1 <*> VFailure e2 = VFailure (e1 <> e2)
  VFailure e1 <*> _ = VFailure e1
  _ <*> VFailure e2 = VFailure e2
  VSuccess f <*> VSuccess a = VSuccess (f a)

-- | Convert a 'Validation' to an 'Either'.
validationToEither :: Validation e a -> Either e a
validationToEither (VFailure e) = Left e
validationToEither (VSuccess a) = Right a

-- ====================================================================
-- ParamError
-- ====================================================================

-- | An error encountered during parameter extraction.
data ParamError
  = -- | A required parameter was not found.
    MissingParam !Text
  | -- | A parameter was present but had the wrong type.
    -- Fields: parameter name, expected type description.
    TypeMismatch !Text !Text
  | -- | A freeform error message.
    ExtraError !Text
  deriving (Eq, Show)

-- ====================================================================
-- ParamParser
-- ====================================================================

-- | Applicative parser that extracts typed parameters from 'Params',
-- accumulating all errors before failing. This enables API consumers
-- to see all missing or malformed parameters in a single error
-- response, not one at a time.
newtype ParamParser a = ParamParser
  { runParamParser :: Params -> Validation [ParamError] a
  }

instance Functor ParamParser where
  fmap f (ParamParser g) = ParamParser (fmap f . g)

instance Applicative ParamParser where
  pure a = ParamParser (\_ -> VSuccess a)
  ParamParser f <*> ParamParser a = ParamParser $ \p ->
    f p <*> a p

-- ====================================================================
-- Combinators
-- ====================================================================

-- | Extract a required named parameter. Fails with 'MissingParam' if
-- the parameter is absent, or 'TypeMismatch' if the JSON value
-- cannot be decoded to the expected type.
--
-- @
-- param \"name\" :: ParamParser Text
-- @
param :: FromJSON a => Text -> ParamParser a
param name = ParamParser $ \case
  ParamsByName km ->
    case KM.lookup (Key.fromText name) km of
      Nothing -> VFailure [MissingParam name]
      Just v -> case fromJSON v of
        Aeson.Success a -> VSuccess a
        Aeson.Error _ -> VFailure [TypeMismatch name (T.pack "expected compatible type")]
  ParamsByPosition _ -> VFailure [ExtraError ("expected named params, got positional (for param: " <> name <> ")")]
  NoParams -> VFailure [MissingParam name]

-- | Extract an optional named parameter, returning a default value
-- when the parameter is absent. Still fails with 'TypeMismatch' if
-- the parameter is present but has the wrong type.
--
-- @
-- optParam \"verbose\" False :: ParamParser Bool
-- @
optParam :: FromJSON a => Text -> a -> ParamParser a
optParam name def = ParamParser $ \case
  ParamsByName km ->
    case KM.lookup (Key.fromText name) km of
      Nothing -> VSuccess def
      Just v -> case fromJSON v of
        Aeson.Success a -> VSuccess a
        Aeson.Error _ -> VFailure [TypeMismatch name (T.pack "expected compatible type")]
  ParamsByPosition _ -> VFailure [ExtraError ("expected named params, got positional (for param: " <> name <> ")")]
  NoParams -> VSuccess def

-- | Parse the entire params via 'FromJSON'. This short-circuits on the
-- first error (standard aeson behavior), unlike 'param' which
-- aggregates. Useful when you have an existing 'FromJSON' instance and
-- do not need error aggregation.
fromJsonParams :: FromJSON a => ParamParser a
fromJsonParams = ParamParser $ \case
  ParamsByName km -> case fromJSON (Object km) of
    Aeson.Success a -> VSuccess a
    Aeson.Error e -> VFailure [ExtraError (T.pack e)]
  ParamsByPosition v -> case fromJSON (Array v) of
    Aeson.Success a -> VSuccess a
    Aeson.Error e -> VFailure [ExtraError (T.pack e)]
  NoParams -> case fromJSON Null of
    Aeson.Success a -> VSuccess a
    Aeson.Error e -> VFailure [ExtraError (T.pack e)]

-- ====================================================================
-- Running
-- ====================================================================

-- | Run a 'ParamParser' against the given 'Params'. On failure,
-- produces an 'invalidParams' error with all errors listed in the
-- data field.
--
-- This is the bridge between the applicative parser and the handler
-- return type.
withNamedParams
  :: Monad m
  => ParamParser a
  -> (a -> m (Either ErrorObject Value))
  -> Params
  -> m (Either ErrorObject Value)
withNamedParams parser handler params =
  case validationToEither (runParamParser parser params) of
    Left errs -> pure (Left (invalidParams (Just (errorsToValue errs))))
    Right a -> handler a

-- | Extract positional params via 'FromJSON'. The entire 'Params'
-- value is decoded as a JSON Array. For 'NoParams', an empty array
-- is used.
withPositionalParams
  :: (Monad m, FromJSON a)
  => (a -> m (Either ErrorObject Value))
  -> Params
  -> m (Either ErrorObject Value)
withPositionalParams handler = \case
  ParamsByPosition v -> case fromJSON (Array v) of
    Aeson.Success a -> handler a
    Aeson.Error e -> pure (Left (invalidParams (Just (Aeson.String (T.pack e)))))
  NoParams -> case fromJSON (Array V.empty) of
    Aeson.Success a -> handler a
    Aeson.Error e -> pure (Left (invalidParams (Just (Aeson.String (T.pack e)))))
  ParamsByName _ ->
    pure (Left (invalidParams (Just (Aeson.String "expected positional params, got named"))))

-- | Render a list of 'ParamError' values as a JSON value for the
-- error's @data@ field.
errorsToValue :: [ParamError] -> Value
errorsToValue errs = Aeson.toJSON (map renderError errs)
  where
    renderError :: ParamError -> Value
    renderError (MissingParam n) = Aeson.String ("missing param: " <> n)
    renderError (TypeMismatch n t) = Aeson.String ("type mismatch for param " <> n <> ": " <> t)
    renderError (ExtraError msg) = Aeson.String msg
