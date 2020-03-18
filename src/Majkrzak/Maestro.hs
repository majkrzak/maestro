module Majkrzak.Maestro
  ( Application(Application)
  , Event
  , load
  , Action
  , dump
  , TopicMapping(TopicMapping)
  , (/-/)
  )
where

import Majkrzak.Maestro.Utils
import Majkrzak.Maestro.Types

import Data.Text (Text, pack)


data TopicMapping = TopicMapping Text Text

(/-/) :: RName a => Text -> a -> TopicMapping
l /-/ r = TopicMapping l $ pack $ name r

data Application = Application
  {
  }
