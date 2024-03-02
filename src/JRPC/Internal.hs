{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JRPC.Internal where

import Data.Aeson
import Data.Text
import Data.Maybe
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits
import Data.Coerce
import qualified JRPC.Types as JT
import Data.Scientific

type Param (a :: Symbol) = JT.Param a (Maybe Value)

type CustomError = JT.CustomError Text Object Int

type ToMethod f = JT.ToMethod f Array Object Text Value Int IO

newtype Method = Method (Either Array Object -> IO (Either CustomError Value)) 

newtype MethodMap = MethodMap (HM.HashMap Text Method)

fromList :: [(Text, Method)] -> MethodMap
fromList = MethodMap . HM.fromList
{-# INLINE fromList #-}

makeCustomError :: Text -> Maybe Object -> Int -> CustomError
makeCustomError = JT.CustomError
{-# INLINE makeCustomError #-}

getParam :: Param a -> Maybe Value
getParam (JT.Param p) = p
{-# INLINE getParam #-}

makeMethod :: ToMethod f => f -> Method
makeMethod = Method . JT.mkMethod pack (KM.lookup . K.fromText) V.uncons 
{-# INLINE makeMethod #-}

run :: MethodMap
    -> Maybe (forall a . V.Vector (IO a) -> IO (V.Vector a))
    -> Value
    -> IO Value
run (MethodMap methodMap) mbStrategy = go True

  where

    go :: Bool -> Value -> IO Value
    go arrayIsAllowed = \case
        Object obj -> fmap responseToJSON $ runOnObject obj
        Array arr | arrayIsAllowed -> runOnArray arr
        _ -> invalidReq
      where
        invalidReq = pure $ jsonFromError JT.InvalidRequest

    runOnArray :: V.Vector Value -> IO Value
    runOnArray = fmap Array . strategy . fmap (go False) 

    responseToJSON
      :: Either
          (JT.JsonRpcError Scientific)
          (Scientific, Either CustomError Value)
      -> Value 
    responseToJSON = either jsonFromError jsonFromResult
      where
        jsonFromResult (id_, res) = case res of
          Left (JT.CustomError m d c) -> mkJsonRpcError (Just id_) m d c
          Right result_ -> mkJsonRpcResult id_ result_

    runOnObject
      :: Object
      -> IO ( Either (JT.JsonRpcError Scientific)
              (Scientific, Either CustomError Value)
            )
    runOnObject obj = do
      fromMaybe (pure $ Left JT.InvalidRequest) do

        id_ <- do
          KM.lookup (K.fromText "id") obj >>= \case
            Number n -> Just n
            _ -> Nothing

        method <- do
          KM.lookup (K.fromText "method") obj >>= \case
            String s -> Just s
            _ -> Nothing

        params <- do
          KM.lookup (K.fromText "params") obj >>= \case
            Object obj_ -> Just $ Right obj_
            Array arr -> Just $ Left arr
            _ -> Nothing
          

        pure $ case HM.lookup method methodMap of
          Nothing -> pure $ Left $ JT.MethodNotFound id_
          Just (Method f) -> fmap (Right . (id_,) . coerce) (f params)

    jsonFromError :: JT.JsonRpcError Scientific -> Value
    jsonFromError = \case
      JT.ParseError        -> mkError "Parse error"             (negate 32700)
      JT.InvalidRequest    -> mkError "Invalid request"         (negate 32600)
      JT.MethodNotFound id_ -> mkErrorId id_ "Method not found" (negate 32601)
      JT.InvalidParams id_  -> mkErrorId id_ "Invalid params"   (negate 32602)
      JT.InternalError id_  -> mkErrorId id_ "Internal error"   (negate 32603)
      where
        mkErrorId id_ message c = mkJsonRpcError (Just id_) message Nothing c
        mkError message c = mkJsonRpcError Nothing message Nothing c

    strategy :: V.Vector (IO a) -> IO (V.Vector a)
    strategy = fromMaybe sequence mbStrategy

    mkJsonRpcError :: Maybe Scientific -> Text -> Maybe Object -> Int -> Value
    mkJsonRpcError mbId message mbData code = object
      [ "id" .= fromMaybe Null (fmap Number mbId),
        "jsonrpc" .= String "2.0",
        "error" .= object
          [ "code" .= Number (realToFrac code),
            "message" .= coerce @_ @Text message,
            "data" .= fromMaybe Null (fmap Object $ coerce mbData)
          ]
      ]

    mkJsonRpcResult :: Scientific -> Value -> Value
    mkJsonRpcResult id_ res = object
      [ "id" .= Number id_,
        "jsonrpc" .= String "2.0",
        "result" .= res
      ]
{-# INLINE run#-}
