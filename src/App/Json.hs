module App.Json
  ( defaultToJSON,
    defaultParseJSON,
  )
where

import Data.Aeson
  ( GFromJSON,
    GToJSON,
    Options (constructorTagModifier, fieldLabelModifier),
    Value,
    Zero,
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic, Rep)

defaultToJSON :: (Generic a, GToJSON Zero (Rep a)) => String -> (a -> Value)
defaultToJSON prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop (length prefix),
        constructorTagModifier = camelTo2 '_' . drop (length prefix)
      }

defaultParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> (Value -> Parser a)
defaultParseJSON prefix =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop (length prefix),
        constructorTagModifier = camelTo2 '_' . drop (length prefix)
      }
