module PrettyJSON
    (
      renderJValue
    ) where

import SimpleJSON (JValue(..))
import Prettify

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

renderJValue (JArray ary) = series '[' ']' renderJValue ary

renderJValue (JObject obj) = series '{' '}' field obj
    where field (k,v) = string k <> text ": " <> renderJValue v