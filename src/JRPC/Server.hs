{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JRPC.Server where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified JRPC.Server.Internal as I
import Data.ByteString.Lazy

type ToMethod a = I.ToMethod a

type Param a = I.Param a

type CustomError = I.CustomError

type Method = I.Method

type MethodMap = I.MethodMap

fromList :: [(Text, Method m)] -> MethodMap m
fromList = I.fromList
{-# INLINE fromList #-}

makeCustomError :: Text -> Maybe Value -> Int -> CustomError
makeCustomError = I.makeCustomError
{-# INLINE makeCustomError #-}

getParam :: Param a -> Maybe Value
getParam = I.getParam
{-# INLINE getParam #-}

makeMethod :: ToMethod f m => f -> Method m
makeMethod = I.makeMethod
{-# INLINE makeMethod #-}

runOnValue
  :: Monad m
  => MethodMap m
  -> Maybe (forall a . Vector (m a) -> m (Vector a))
  -> Value
  -> m (Maybe Value)
runOnValue  = I.runOnValue
{-# INLINE runOnValue #-}

run
  :: Monad m
  => MethodMap m
  -> Maybe (forall a . Vector (m a) -> m (Vector a))
  -> ByteString
  -> m (Maybe Value)
run = I.run
{-# INLINE run #-}
