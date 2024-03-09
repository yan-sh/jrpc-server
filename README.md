# JRPC-Server: A simplest implementation of json-rpc server

A library to create json-rpc server in the simplest way

# Example

```haskell
import JRPC
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

methodMap :: MethodMap
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

```

```console
$ cabal run
Object (fromList [("id",Number 1.0),("jsonrpc",String "2.0"),("result",String "321")])
Object (fromList [("id",Number 1.0),("jsonrpc",String "2.0"),("result",String "321")])
Object (fromList [("error",Object (fromList [("code",Number (-32600.0)),("data",Null),("message",String "Invalid request")])),("id",Null),("jsonrpc",String "2.0")])
```
