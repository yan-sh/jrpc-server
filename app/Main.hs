{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

import JRPC.Server
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

methodMap :: MethodMap IO
methodMap = fromList
  [ ( "reverse"
    , makeMethod reverseMethod
    )
  ]
    where
      reverseMethod
        :: Param "string"
        -> IO (Either CustomError Value)

      reverseMethod (getParam -> Nothing) = pure do
        Left $ makeCustomError "empty" Nothing 400

      reverseMethod (getParam -> Just json_) = pure do
        case json_ of
          String s -> Right $ String (T.reverse s)
          _ -> Left $ makeCustomError "wrong json" Nothing 400

main :: IO ()
main = do

  print =<< do
    run methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      , "params" .= object ["string" .= String "123"]
      ]

  print =<< do
    run methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      , "params" .= Array (V.fromList [String "123"])
      ]

  print =<< do
    run methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      ]

  print =<< do
    run methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "method" .= String "reverse"
      , "params" .= Array (V.fromList [String "123"])
      ]
