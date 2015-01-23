{-# LANGUAGE NoImplicitPrelude, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module ProtoRec where

import BasePrelude
import GHC.TypeLits

data Named (n :: Symbol) v = Named v deriving Show

data Field (n :: Symbol) = Field

-- We suppose that all names should be different for whole Product and for whole Sum
-- To express it in type we need FD. But it leads to UndecidableInstances

-- 1. AnonProduct
class FieldOwner (n :: Symbol) v a where
    -- -- | n a -> v where 
    -- with FD we need UndecidableInstances also
  setField :: Field n -> v -> a -> a
  getField :: Field n -> a -> v

instance FieldOwner n v (Named n v) where
    setField _ v' (Named v) = Named v'
    getField _ (Named v)    = v

instance (FieldOwner n v (Named n1 v1)) => FieldOwner n v (Named n1 v1, a) where
    setField n v    = first $ setField n v
    getField n      = getField n . fst

instance (FieldOwner n v (Named n1 v1)) => FieldOwner n v (a, Named n1 v1) where
    setField n v    = second $ setField n v
    getField n      = getField n . snd

instance (FieldOwner n v (a,b)) => FieldOwner n v ((a,b), c) where
    setField n v    = first $ setField n v
    getField n      = getField n . fst

instance (FieldOwner n v (a,b)) => FieldOwner n v (c, (a,b)) where
    setField n v    = second $ setField n v
    getField n      = getField n . snd

-- We got access at O(log2(n)). If we want we can do O(logk(n)) for any k. 2*k+1 instances are required then.

-- 2. How to make Applicative style?

-- Person ((Named "name" String, Named "surname" String), Named "age" Int)
-- Person <$> getName <*> getAge



-- 3. AnonSum

class ConsOwner (n :: Symbol) v a where
    construct   :: Field n -> v -> a
    deconstruct :: Field n -> a -> Maybe v

instance ConsOwner n v (Named n v) where
    construct   _ v         = Named v
    deconstruct _ (Named v) = Just v

instance (ConsOwner n v (Named n1 v1)) => ConsOwner n v (Either (Named n1 v1) a) where
    construct   n v         = Left $ construct n v
    deconstruct n           = either (deconstruct n) (const Nothing)

instance (ConsOwner n v (Named n1 v1)) => ConsOwner n v (Either a (Named n1 v1)) where
    construct   n v         = Right $ construct n v
    deconstruct n           = either (const Nothing) (deconstruct n)

instance (ConsOwner n v (Either a b)) => ConsOwner n v (Either (Either a b) c) where
    construct   n v         = Left $ construct n v
    deconstruct n           = either (deconstruct n) (const Nothing)

instance (ConsOwner n v (Either a b)) => ConsOwner n v (Either c (Either a b)) where
    construct   n v         = Right $ construct n v
    deconstruct n           = either (const Nothing) (deconstruct n)
