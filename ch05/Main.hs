module Main where

import PrettyJSON
import SimpleJSON (JValue(..))
import Prettify (compact, pretty)

myObject :: JValue
myObject = JObject [
      ("foo", JNumber 1)
    , ("bar", JBool False)
    , ("baz", JArray [JString "Hello", JString "World"])
    ]

main = putStrLn $ compact (renderJValue myObject)