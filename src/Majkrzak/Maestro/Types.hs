module Majkrzak.Maestro.Types
  ( Event
  , load
  , Action
  , dump
  )
where

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
  , Value(Object)
  , fieldLabelModifier
  , constructorTagModifier
  , omitNothingFields
  , unwrapUnaryRecords
  , tagSingleConstructors
  , GToJSON
  , genericToJSON
  , Options
  )
import Data.Maybe (fromMaybe)
import Data.Aeson (decode, encode)
import Data.HashMap.Strict (toList)

aesonOptions :: Options
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

class Action a where
  dump :: a -> (Text, ByteString)
  default dump ::
   ( Generic a
   , GToJSON Zero (Rep a)
   ) => a -> (Text, ByteString)
  dump a = (tmp1, encode tmp2)
    where
      (tmp1, tmp2) = head $ toList tmp3
      (Object tmp3) = genericToJSON aesonOptions a
