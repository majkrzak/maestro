module Majkrzak.Maestro
  ( Application(Application)
  , Event
  , load
  , Action
  , dump
  , TopicMapping(TopicMapping)
  , (/+/)
  , (/-/)
  , runApplication
  )
where

import Majkrzak.Maestro.Utils
import Majkrzak.Maestro.Types

import Prelude (Bool(True, False), IO, ($), (.), filter, map)
import Data.Text (Text, pack)
import Network.MQTT.Client
  ( connectURI
  , _msgCB
  , mqttConfig
  , MessageCallback(SimpleCallback)
  , publish
  , waitForClient
  , subscribe
  , subOptions
  )
import Network.URI (URI)
import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar)
import Control.Monad (void, mapM_, return)


data TopicMapping = TopicMapping Bool Text Text

(/+/) :: RName a => Text -> a -> TopicMapping
l /+/ r = TopicMapping True l $ pack $ name r

(/-/) :: RName a => Text -> a -> TopicMapping
l /-/ r = TopicMapping False l $ pack $ name r

data Application s e a = Application
  { init :: s
  , next :: (Event e, Action a) => s -> e -> (s, [a])
  , maps :: [TopicMapping]
  , mqtt :: URI
  }


runApplication :: (Event e, Action a) => Application s e a -> IO ()
runApplication Application {..} = do
  state <- atomically . newTVar $ init
  mc    <- connectURI
    mqttConfig
      { _msgCB = SimpleCallback $ \mc topic payload _ -> do
        event   <- load topic payload
        actions <- atomically $ do
          oldState <- readTVar state
          let (newState, actions) = next oldState event
          writeTVar state newState
          return actions
        mapM_
          ((\(topic', payload') -> publish mc topic' payload' False) . dump)
          actions
      }
    mqtt
  let filtered = filter (\(TopicMapping s _ _) -> s) maps
  let mapped   = map (\(TopicMapping _ t _) -> (t, subOptions)) filtered
  void $ subscribe mc mapped []
  waitForClient mc
