{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JRPC.Server.Internal where

import Data.Aeson
import Data.Text ( Text )
import Data.ByteString.Lazy
import Data.Maybe ( fromMaybe )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Coerce ( coerce )
import qualified JRPC.Types as JT

type Param = JT.Param

type CustomError = JT.CustomError

type ToMethod = JT.ToMethod

type Method = JT.Method

type MethodMap = JT.MethodMap

type Response = Either (JT.JsonRpcError Id) (Id, Either CustomError Value)

data Id = IdNumber !Int | IdString !Text

idToValue :: Id -> Value
idToValue = \case
  IdNumber n -> Number (fromInteger $ toInteger n)
  IdString s -> String s
{-# INLINE idToValue #-}

fromList :: [(Text, Method m)] -> MethodMap m
fromList = JT.MethodMap . HM.fromList
{-# INLINE fromList #-}

makeCustomError :: Text -> Maybe Value -> Int -> CustomError
makeCustomError = JT.CustomError
{-# INLINE makeCustomError #-}

getParam :: Param a -> Maybe Value
getParam (JT.Param p) = p
{-# INLINE getParam #-}

makeMethod :: ToMethod f m => f -> Method m
makeMethod = JT.Method . JT.mkMethod
{-# INLINE makeMethod #-}

jsonFromError :: JT.JsonRpcError Id -> Value
jsonFromError = \case
  JT.ParseError        -> mkError "Parse error"             (negate 32700)
  JT.InvalidRequest    -> mkError "Invalid request"         (negate 32600)
  JT.MethodNotFound id_ -> mkErrorId id_ "Method not found" (negate 32601)
  JT.InvalidParams id_  -> mkErrorId id_ "Invalid params"   (negate 32602)
  JT.InternalError id_  -> mkErrorId id_ "Internal error"   (negate 32603)
  where
    mkErrorId id_ message c = mkJsonRpcError (Just id_) message Nothing c    
    mkError message c = mkJsonRpcError Nothing message Nothing c
{-# INLINE jsonFromError #-}

mkJsonRpcError :: Maybe Id -> Text -> Maybe Value -> Int -> Value
mkJsonRpcError mbId message mbData code = object
  [ "id" .= maybe Null idToValue mbId,
    "jsonrpc" .= String "2.0",
    "error" .= object
      [ "code" .= Number (realToFrac code),
        "message" .= coerce @_ @Text message,
        "data" .= fromMaybe Null mbData
      ]
  ]
{-# INLINE mkJsonRpcError #-}

run :: forall m . Monad m
    => MethodMap m
    -> Maybe (forall a . V.Vector (m a) -> m (V.Vector a))
    -> ByteString
    -> m (Maybe Value)
run mm mbStrategy bs = do
  case decode' bs of
    Nothing -> pure $ Just $ jsonFromError JT.ParseError
    Just v -> runOnValue mm mbStrategy v
{-# INLINE run #-}

responseToJSON :: Response -> Value
responseToJSON = either jsonFromError jsonFromResult
  where
    jsonFromResult (id_, res) = case res of
      Left (JT.CustomError m d c) -> mkJsonRpcError (Just id_) m d c
      Right result_ -> mkJsonRpcResult id_ result_
{-# INLINE responseToJSON #-}

mkJsonRpcResult :: Id -> Value -> Value
mkJsonRpcResult id_ res = object
  [ "id" .= idToValue id_,
    "jsonrpc" .= String "2.0",
    "result" .= res
  ]
{-# INLINE mkJsonRpcResult #-}

runOnValue
  :: forall m . Monad m
  => MethodMap m
  -> Maybe (forall a . V.Vector (m a) -> m (V.Vector a))
  -> Value
  -> m (Maybe Value)
runOnValue (JT.MethodMap methodMap) mbStrategy = go True

  where

    go :: Bool -> Value -> m (Maybe Value)
    go arrayIsAllowed = \case
        Object obj -> fmap responseToJSON <$> runOnObject obj
        Array arr | arrayIsAllowed -> fmap Just (runOnArray arr)
        _ -> invalidReq
      where
        invalidReq = pure $ Just $ jsonFromError JT.InvalidRequest

    runOnArray :: V.Vector Value -> m Value
    runOnArray =
        fmap (Array . V.fromList . V.foldr (\x acc -> maybe acc (:acc) x) [])
      . strategy
      . fmap (go False)

    runOnObject :: Object -> m (Maybe Response)
    runOnObject obj = go_ mbMethod mbParams

      where

        go_ :: Maybe Text -> Maybe (Either Array Object) -> m (Maybe Response) 
        go_ (Just method) (Just params) = do
          case HM.lookup method (methodMap :: HM.HashMap Text (JT.Method m)) of
            Just (JT.Method f) -> do
              !res <- f params
              pure $ withMbId $ Right . (, coerce res)
            Nothing -> pure $ withMbId $ Left . JT.MethodNotFound

        go_ _ _ = pure $ Just $ Left JT.InvalidRequest

        withMbId f =
          KM.lookup (K.fromText "id") obj >>= \case
            Number n -> Just $ f $ IdNumber (round n)
            String s -> Just $ f $ IdString s
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

    strategy = fromMaybe sequence mbStrategy

{-# INLINE runOnValue #-}
