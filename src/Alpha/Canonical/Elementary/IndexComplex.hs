-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Elementary.IndexComplex
(
    module X,
    IndexPosition(..),
    PositionedIndex(..),
    UpperIndex(..),
    LowerIndex(..),
    IndexInfo(..),
    IndexComplex(..),
    ixpos,
    ixlower,
    ixupper,
    MultiIndex(..),
    MultiIndexed(..),
    testIndex,

)
where

import Alpha.Canonical.Elementary.Common as X
import Alpha.Canonical.Elementary.Tuples as X
import Alpha.Canonical.Elementary.IndexedTerm as X


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
        
type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = UniTuple1 (IndexRange a)
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

-- | Characterizes a multi-level index    
class  (KnownNat i, OrdEnum a) => MultiIndexed i a where
    ixmulti::UniTuple i (IndexRange a) -> MultiIndex i a
    ixlevels::MultiIndex i a -> [UniTuple i a]

    
-- | Determines the index position characteristic
ixpos::PositionedIndex s n -> IndexPosition
ixpos (Lower _) = LowerPosition
ixpos (Upper _) = UpperPosition

-- | Constructs a lower index
ixlower::forall s n. SymNat s n => PositionedIndex s n
ixlower = Lower (LowerIndex @s @n)

-- | Constructs n upper index
ixupper::forall s n. SymNat s n => PositionedIndex s n
ixupper = Upper (UpperIndex @s @n)
                        
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

instance (OrdEnum a) => MultiIndexed 1 a where    
    ixmulti (UniTuple1 r) =  UniTuple1 r
    ixlevels (UniTuple1 r) = [UniTuple1 a | a <- associates r]

instance (OrdEnum a) => MultiIndexed 2 a where    
    ixmulti (r1, r2) = (r1, r2)
    ixlevels (r1, r2) 
        = [(a1,a2) | a1 <- associates r1, a2 <- associates r2]
                    
instance (OrdEnum a) => MultiIndexed 3 a where    
    ixmulti (r1, r2, r3) = (r1 , r2, r3)
    ixlevels (r1, r2, r3) 
        = [(a1,a2,a3) | a1 <- associates r1, a2 <- associates r2, a3 <- associates r3]

instance (OrdEnum a) => MultiIndexed 4 a where    
    ixmulti (r1, r2, r3, r4) = (r1 , r2, r3, r4)
    ixlevels (r1, r2, r3, r4) 
        =  [(a1,a2,a3,a4) | 
                a1 <- associates r1, 
                a2 <- associates r2, 
                a3 <- associates r3, 
                a4 <- associates r4]

instance (OrdEnum a) => MultiIndexed 5 a where    
    ixmulti (r1, r2, r3, r4, r5) = (r1 , r2, r3, r4, r5)
    ixlevels (r1, r2, r3, r4, r5) 
        =  [(a1,a2,a3,a4,a5) | 
                a1 <- associates r1, 
                a2 <- associates r2, 
                a3 <- associates r3, 
                a4 <- associates r4, 
                a5 <- associates r5]
    
testIndex = mi where
    r1 = ixrange (1,10)
    r2 = ixrange (20,25)
    r3 = ixrange (50,60)
    mi = ixmulti @3 (r1,r2,r3)
                    