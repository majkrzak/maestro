module Majkrzak.Maestro 
  (Event
  , load
  , TopicMapping(TopicMapping)
  , (/-/)
) where

import Majkrzak.Maestro.Utils
import Majkrzak.Maestro.Types

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


data TopicMapping = TopicMapping Text Text

(/-/) :: RName a => Text -> a -> TopicMapping
l /-/ r = TopicMapping l $ pack $ name r
