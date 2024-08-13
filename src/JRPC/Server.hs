{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JRPC.Server where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified JRPC.Server.Internal as I

type ToMethod a = I.ToMethod a

type Param a = I.Param a

type CustomError = I.CustomError

type Method = I.Method

type MethodMap = I.MethodMap

fromList :: [(Text, Method)] -> MethodMap
fromList = I.fromList
{-# INLINE fromList #-}

makeCustomError :: Text -> Maybe Value -> Int -> CustomError
makeCustomError = I.makeCustomError
{-# INLINE makeCustomError #-}

getParam :: Param a -> Maybe Value
getParam = I.getParam
{-# INLINE getParam #-}

makeMethod :: ToMethod f IO => f -> Method
makeMethod = I.makeMethod
{-# INLINE makeMethod #-}

run :: MethodMap
    -> Maybe (forall a . Vector (IO a) -> IO (Vector a))
    -> Value
    -> IO (Maybe Value)
run = I.run
{-# INLINE run #-}
