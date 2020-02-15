module Majkrzak.Maestro.Utils
  ( RName
  , name
  )
where

import GHC.Generics


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
