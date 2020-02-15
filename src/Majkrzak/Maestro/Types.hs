module Majkrzak.Maestro.Types
  ( Event
  , load
  )
where

import Control.Monad.Fail (MonadFail)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic, Rep)
import Data.Aeson.Types
  ( GFromJSON
  , Zero
  , Result(Success, Error)
  , parse
  , genericParseJSON
  , object
  , emptyArray
  , (.=)
  , sumEncoding
  , allNullaryToStringTag
  , SumEncoding(ObjectWithSingleField)
  , defaultOptions
  , Value(String)
  , fieldLabelModifier
  , constructorTagModifier
  , omitNothingFields
  , unwrapUnaryRecords
  , tagSingleConstructors
  )
import Data.Maybe (fromMaybe)
import Data.Aeson (decode)


aesonOptions = defaultOptions
  { fieldLabelModifier     = id
  , constructorTagModifier = id
  , allNullaryToStringTag  = False
  , omitNothingFields      = True
  , sumEncoding            = ObjectWithSingleField
  , unwrapUnaryRecords     = False
  , tagSingleConstructors  = True
  }

class Event a where
  load :: MonadFail m => Text -> ByteString -> m a
  default load ::
    ( MonadFail m
    , Generic a
    , GFromJSON Zero (Rep a)
    ) => Text -> ByteString -> m a
  load t x =
    case parse
        (genericParseJSON aesonOptions)
        (object [t .= emptyArray])
    of
      Success s -> return s
      Error _ -> case parse
          (genericParseJSON aesonOptions)
          (object [t .= fromMaybe emptyArray (decode x)])
        of
          Error e -> fail e
          Success s -> return s
