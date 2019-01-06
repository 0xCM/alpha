-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Elementary.Indexed
(
    module X,
    IndexedElement(..),
    Indexed(..),
    NaturallyIndexed(..),
    SafeIndex(..),
    PositionedIndex(..),
    IndexInfo(..),
    lowerIndex,
    upperIndex,
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Set as X


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence

-- | Represents a lower index
data LowerIndex s n = LowerIndex
    deriving (Eq, Ord)

-- | Represents an upper index
data UpperIndex s n = UpperIndex
    deriving (Eq, Ord)

-- | Specifies possible index positions
data IndexPosition =
      UpperPosition
    | LowerPosition
    deriving (Eq, Ord)

-- | Unifies  'LowerIndex' and 'UpperIndex' types
data PositionedIndex s n =
      Lower (LowerIndex s n)
    | Upper (UpperIndex s n)
    deriving (Eq, Ord)

-- | Describes a 'PositionedIndex' at the value-level    
newtype IndexInfo = IndexInfo (IndexPosition, Text, Word)
    deriving (Eq, Ord,Generic)

-- | Defines a collection of index complexes as a unit that consists
-- of an ordered pair of contravariant and covariant components    
newtype IndexComplex = IndexComplex ([IndexInfo],[IndexInfo])
    deriving (Eq, Ord,Generic)
        
type family IndexedElement (i::k) a
type instance IndexedElement a (Map a b) = (a,b)
type instance IndexedElement Int (Vector a) = a
type instance IndexedElement Int (Seq a) = a
type instance IndexedElement Int [a] = a

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed i a where

    at::a -> i -> IndexedElement i a

    (!)::a -> i -> IndexedElement i a
    (!) = at            
    infixr 9 !

class SafeIndex s i where
    
    lookup::s -> i -> Maybe (IndexedElement i s)

    (!?)::s -> i -> Maybe (IndexedElement i s)
    (!?) = lookup
    infixr 9 !?

-- | Characterizes an element indexed via type-level naturals    
class KnownNat i => NaturallyIndexed i a where
    natix::a -> IndexedElement i a

-- | Determines the index position characteristic
ixpos::PositionedIndex s n -> IndexPosition
ixpos (Lower _) = LowerPosition
ixpos (Upper _) = UpperPosition

lowerIndex::forall s n. SymNat s n => PositionedIndex s n
lowerIndex = Lower (LowerIndex @s @n)

upperIndex::forall s n. SymNat s n => PositionedIndex s n
upperIndex = Upper (UpperIndex @s @n)

instance (Eq a) => Indexed Int [a] where    
    at = (List.!!)

instance Indexed Int (Seq a) where
    at = Sequence.index
    
instance Indexed Int (Vector a) where
    at = (Vector.!)
    
instance (Ord k) => Indexed k (Map k v) where
    at map k = (k, map Map.! k)

instance (Ord k) => SafeIndex (Map k v) k where
     lookup map k = case (map Map.!? k)of
                        Just v -> Just (k, v)
                        _      -> Nothing
                        
instance forall s n. SymNat s n  => Specifiable (PositionedIndex s n) where
    type Specified (PositionedIndex s n) = IndexInfo
    specify idx = IndexInfo (ixpos idx, symtext @s, integral $ nat @n)

instance Formattable IndexInfo where
    format (IndexInfo (v,label,pos)) = format (v, label, pos)

instance Show IndexInfo where
    show = string . format

instance SymNat s n =>  Formattable (PositionedIndex s n) where
    format = format . specify

instance SymNat s n => Show (PositionedIndex s n) where
    show = string . format
    
instance Formattable IndexPosition where
    format (LowerPosition) = "lower"
    format (UpperPosition) = "upper"
    
instance Show IndexPosition where
    show = string . format    
