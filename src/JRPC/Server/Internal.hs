{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JRPC.Server.Internal where

import Data.Aeson
import Data.Text ( Text )
import Data.Maybe ( fromMaybe )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Coerce ( coerce )
import qualified JRPC.Types as JT
import Data.Scientific ( Scientific )

type Param = JT.Param

type CustomError = JT.CustomError

type ToMethod = JT.ToMethod

type Method = JT.Method

type MethodMap = JT.MethodMap

fromList :: [(Text, Method)] -> MethodMap
fromList = JT.MethodMap . HM.fromList
{-# INLINE fromList #-}

makeCustomError :: Text -> Maybe Value -> Int -> CustomError
makeCustomError = JT.CustomError
{-# INLINE makeCustomError #-}

getParam :: Param a -> Maybe Value
getParam (JT.Param p) = p
{-# INLINE getParam #-}

makeMethod :: ToMethod f IO => f -> Method
makeMethod = JT.Method . JT.mkMethod
{-# INLINE makeMethod #-}

run :: MethodMap
    -> Maybe (forall a . V.Vector (IO a) -> IO (V.Vector a))
    -> Value
    -> IO (Maybe Value)
run (JT.MethodMap methodMap) mbStrategy = go True

  where

    go :: Bool -> Value -> IO (Maybe Value)
    go arrayIsAllowed = \case
        Object obj -> fmap responseToJSON <$> runOnObject obj
        Array arr | arrayIsAllowed -> fmap Just (runOnArray arr)
        _ -> invalidReq
      where
        invalidReq = pure $ Just $ jsonFromError JT.InvalidRequest

    runOnArray :: V.Vector Value -> IO Value
    runOnArray =
        fmap (Array . V.fromList . V.foldr (\x acc -> maybe acc (:acc) x) [])
      . strategy
      . fmap (go False)

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
      -> IO ( Maybe
              ( Either
                  (JT.JsonRpcError Scientific)
                  (Scientific, Either CustomError Value)
              )
            )
    runOnObject obj = go_ mbId mbMethod mbParams

      where

        go_ mbId_ (Just method) (Just params) = do
          case HM.lookup method methodMap of
            Just (JT.Method f) -> do
              !res <- f params
              pure do
                case mbId_ of
                  Nothing -> Nothing
                  Just id_ -> Just $ Right $ (id_, coerce res)
            Nothing -> pure do
              case mbId_ of
                Nothing -> Nothing
                Just id_ -> Just $ Left $ JT.MethodNotFound id_

        go_ _ _ _ = pure $ Just $ Left JT.InvalidRequest

        mbId =
          KM.lookup (K.fromText "id") obj >>= \case
            Number n -> Just n
            _ -> Nothing

        mbMethod =
          KM.lookup (K.fromText "method") obj >>= \case
            String s -> Just s
            _ -> Nothing

        mbParams =
          KM.lookup (K.fromText "params") obj >>= \case
            Object obj_ -> Just $ Right obj_
            Array arr -> Just $ Left arr
            _ -> Nothing

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

    mkJsonRpcError :: Maybe Scientific -> Text -> Maybe Value -> Int -> Value
    mkJsonRpcError mbId message mbData code = object
      [ "id" .= maybe Null Number mbId,
        "jsonrpc" .= String "2.0",
        "error" .= object
          [ "code" .= Number (realToFrac code),
            "message" .= coerce @_ @Text message,
            "data" .= fromMaybe Null mbData
          ]
      ]

    mkJsonRpcResult :: Scientific -> Value -> Value
    mkJsonRpcResult id_ res = object
      [ "id" .= Number id_,
        "jsonrpc" .= String "2.0",
        "result" .= res
      ]
{-# INLINE run#-}
