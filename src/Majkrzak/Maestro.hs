module Majkrzak.Maestro 
  (Event
  , load
  , TopicMapping(TopicMapping)
  , (/-/)
) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Data.Aeson.Types (GFromJSON, GToJSON, Zero, sumEncoding, allNullaryToStringTag, object, (.=), emptyArray, genericParseJSON, defaultOptions, SumEncoding(ObjectWithSingleField), parse, Result(Error, Success), genericToJSON, Value(Object))
import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.Aeson (decode, encode)
import Control.Monad.Fail (MonadFail)
import Data.HashMap.Strict (toList)
import Data.Data (toConstr, Data)


aesonOptions = defaultOptions
 { sumEncoding = ObjectWithSingleField
 , allNullaryToStringTag = False
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
      (Object tmp3) = genericToJSON defaultOptions a


class GName f where
  name' :: f p -> String

instance GName f => GName (D1 c f) where
  name' (M1 x) = name' x

instance (GName l, GName r) => GName (l :+: r) where
  name' (L1 l) = name' l
  name' (R1 r) = name' r

instance Constructor c => GName (C1 c f) where
  name' = conName 

class RName a where
  name :: a -> String

instance  {-# OVERLAPPING #-}  RName a => RName (r -> a) where
  name f = name (f undefined)

instance (Generic a, GName (Rep a)) => RName a where
  name = name' . from

data TopicMapping = TopicMapping Text Text

(/-/) :: RName a => Text -> a -> TopicMapping
l /-/ r = TopicMapping l $ pack $ name r
