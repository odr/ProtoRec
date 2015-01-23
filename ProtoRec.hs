{-# LANGUAGE DataKinds, FlexibleInstances #-}
-- , FunctionalDependencies, UndecidableInstances #-}
-- , ScopedTypeVariables, , OverlappingInstances #-}
module ProtoRec where

import BasePrelude -- hiding (Proxy)
import GHC.TypeLits

data Record (n :: Symbol) v = Record v deriving Show
-- data Record2 (n1 :: Symbol) v1 (n2 :: Symbol) v2 = Record2 v1 v2 deriving Show
-- data Record4 (n1 :: Symbol) v1 (n2 :: Symbol) v2 (n3 :: Symbol) v3 (n4 :: Symbol) v4 = Record4 v1 v2 v3 v4 deriving Show

data Field (t :: Symbol) = Field

class FieldOwner (n :: Symbol) v a where -- -  | n a -> v where 
  setField :: Field n -> v -> a -> a
  getField :: Field n -> a -> v

instance FieldOwner n v (Record n v) where
    setField _ v' (Record v)    = Record v'
    getField _ (Record v)       = v

{-
instance (FieldOwner n v (Record1 n1 v1)) => FieldOwner n v (Record2 n1 v1 n2 v2) where
    setField n v (Record2 v1 v2) = (\(Record1 v1') -> Record2 v1' v2) $ setField n v (Record1 v1 :: Record1 n1 v1)
    getField n  (Record2 v1 v2) = getField n (Record1 v1 :: Record1 n1 v1)
instance FieldOwner n2 v2 (Record2 n1 v1 n2 v2) where
    setField _ v (Record2 v1 v2) = Record2 v1 v
    getField _ (Record2 v1 v2)   = v2
-}

instance (FieldOwner n v (Record n1 v1)) => FieldOwner n v (Record n1 v1, a) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v (Record n1 v1)) => FieldOwner n v (a, Record n1 v1) where
    setField n v = second $ setField n v
    getField n = getField n . snd

instance (FieldOwner n v (Record n1 v1, Record n2 v2)) => FieldOwner n v ((Record n1 v1, Record n2 v2), a) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v (Record n1 v1, Record n2 v2)) => FieldOwner n v (a, (Record n1 v1, Record n2 v2)) where
    setField n v = second $ setField n v
    getField n = getField n . snd

instance (FieldOwner n v ((Record n1 v1, Record n2 v2), (Record n3 v3, Record n4 v4))) 
    => FieldOwner n v (((Record n1 v1, Record n2 v2), (Record n3 v3, Record n4 v4)), a) where
    setField n v = first $ setField n v
    getField n = getField n . fst

instance (FieldOwner n v ((Record n1 v1, Record n2 v2), (Record n3 v3, Record n4 v4))) 
    => FieldOwner n v (a, ((Record n1 v1, Record n2 v2), (Record n3 v3, Record n4 v4))) where
    setField n v = second $ setField n v
    getField n = getField n . snd
