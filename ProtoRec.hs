{-# LANGUAGE NoImplicitPrelude, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module ProtoRec where

import BasePrelude
import GHC.TypeLits

data Record (n :: Symbol) v = Record v deriving Show

data Field (t :: Symbol) = Field

class FieldOwner (n :: Symbol) v a where
    -- -- | n a -> v where 
    -- with FD we need UndecidableInstances also
  setField :: Field n -> v -> a -> a
  getField :: Field n -> a -> v

instance FieldOwner n v (Record n v) where
    setField _ v' (Record v)    = Record v'
    getField _ (Record v)       = v

instance (FieldOwner n v (Record n1 v1)) => FieldOwner n v (Record n1 v1, a) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v (Record n1 v1)) => FieldOwner n v (a, Record n1 v1) where
    setField n v = second $ setField n v
    getField n = getField n . snd

{-
instance (FieldOwner n v (Record n1 v1, Record n2 v2)) => FieldOwner n v ((Record n1 v1, Record n2 v2), a) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v (Record n1 v1, Record n2 v2)) => FieldOwner n v (a, (Record n1 v1, Record n2 v2)) where
    setField n v = second $ setField n v
    getField n = getField n . snd
-}

instance (FieldOwner n v (a,b)) => FieldOwner n v ((a,b), c) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v (a,b)) => FieldOwner n v (c, (a,b)) where
    setField n v = second $ setField n v
    getField n = getField n . snd


