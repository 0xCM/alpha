{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Element(Element(..),Membership(..),Set(..),InvariantSet(..))
where
import Alpha.Base

type family Element a
type instance Element (ItemSet a) = a
type instance Element [a] = a
type instance Element (Bag a) = a
type instance Element (Seq a) = a
type instance Element (Stream a) = a
type instance Element (Map a b) = (a,b)
type instance Element (Vector a) = a

class Set a where

    

-- | Characterizes a structure that can exhibit a list of elements
class Membership s where
    members::s -> ItemSet (Element s)         

class InvariantSet s where
    invariants::[s]

instance Set (ItemSet a)
instance Set (InvariantSet a)
instance Set (Membership a)
