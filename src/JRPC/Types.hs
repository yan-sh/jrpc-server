{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module JRPC.Types where

import GHC.TypeLits
import GHC.Prim
import Prelude hiding (lookup)

data CustomError s o c = CustomError s (Maybe o) c

data JsonRpcError n =
    ParseError
  | InvalidRequest
  | MethodNotFound n
  | InvalidParams n
  | InternalError n

newtype Param (name :: Symbol) a = Param a

class ToMethod f array object string json code m where
  mkMethod
    :: (String -> string)
    -> (string -> object -> Maybe json)
    -> (array -> Maybe (json, array))
    -> f
    -> (Either array object -> m (Either (CustomError string object code) json))

instance 
    ( Applicative m
    , ToMethodArray x object string array json code m
    , ToMethodObject x object string json code m
    ) => ToMethod x array object string json code m where
  mkMethod fromString lookup split f = either
    do mkMethodArray @x split f
    do mkMethodObject @x fromString lookup f
  {-# INLINE mkMethod #-}


class ToMethodObject f object string json code m where
  mkMethodObject
    :: (String -> string)
    -> (string -> object -> Maybe json)
    -> f
    -> (object -> m (Either (CustomError string object code) json))

instance (KnownSymbol n, Applicative m, ToMethodObject fs object string json code m) => ToMethodObject (Param n (Maybe json) -> fs) object string json code m where
  mkMethodObject fromString lookup f = \object ->
    mkMethodObject @fs fromString lookup
      (f $ Param $ lookup (fromString $ symbolVal' (proxy# @n)) object)
      object
  {-# INLINE mkMethodObject #-}

instance ToMethodObject (m (Either (CustomError string object code) json)) object string json code m where
  mkMethodObject _ _ = const
  {-# INLINE mkMethodObject #-}



class ToMethodArray f object string array json code m where
  mkMethodArray
    :: (array -> Maybe (json, array))
    -> f
    -> (array -> m (Either (CustomError string object code) json))

instance (KnownSymbol n, Applicative m, ToMethodArray fs object string array json code m) => ToMethodArray (Param n (Maybe json) -> fs) object string array json code m where
  mkMethodArray split f = \array ->
      case split array of
        Nothing       -> mkMethodArray @fs split (f (Param Nothing)) array
        Just (h, ps)  -> mkMethodArray @fs split (f (Param $ Just h)) ps
  {-# INLINE mkMethodArray #-}

instance ToMethodArray (m (Either (CustomError string object code) json)) object string array json code m where
  mkMethodArray _ = const
  {-# INLINE mkMethodArray #-}
