-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Elementary.Indexing
(
    NatIx(..),
    SafeIx(..),
    Indexable(..),
    IxTerm(..), 
    IxRange(..),    
    IxFamily(..),
    UIx(..),
    LIx(..),
    MIx(..),
    BiIx(..),
    MultiIndexed(..),
    lix,
    uix,
    bix,
    term,
    termix,
    termval,
    ixrange,

) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Set as X

import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream
import qualified Data.Sequence as Sequence

-- | Represents a lower index
newtype LIx a = LIx a
    deriving (Eq, Ord,Generic,Data,Typeable)

-- | Represents an upper index
newtype UIx a = UIx a
    deriving (Eq, Ord,Generic,Data,Typeable)

-- | Represents a bipartite index consisting (potentially) of both
-- lower indexes and upper indexes
newtype BiIx a = BiIx ([LIx a],[UIx a])
    deriving (Eq, Ord,Generic,Data,Typeable)
        
-- | Defines a family of multi-indexes    
type family MIx (i::Nat) a = r | r -> i a where
    MIx 1 a = UniTuple1 (IxRange a)
    MIx 2 a = UniTuple2 (IxRange a)
    MIx 3 a = UniTuple3 (IxRange a)
    MIx 4 a = UniTuple4 (IxRange a)
    MIx 5 a = UniTuple5 (IxRange a)

-- | Represents a term t indexed by i
-- A term in this context can be considered a function/delayed 
-- computation predicated on an index i that saturates 
-- when the index is itself applied
newtype IxTerm i t = IxTerm (i, (i -> t))
    deriving (Generic)
instance Newtype (IxTerm i t)

-- | Represents a family 't' of terms indexed by a set 'i'
-- See https://en.wikipedia.org/wiki/Indexed_family
newtype IxFamily i t = IxFamily (Map i (IxTerm i t))
    deriving (Eq, Ord, Generic, Functor)    
instance Newtype (IxFamily i t)    

-- | Defines the lower and upper bounds for an index
newtype IxRange i = IxRange (i,i)
    deriving (Eq,Ord,Generic,Functor)
instance Newtype (IxRange i)    

type instance Individual (IxFamily i t) = IxTerm i t
type instance Individual (IxRange i) = i

class Indexable a where
    type Indexer a
    type Indexer a = Int
    
    idx::a -> Indexer a -> Individual a
    idx = (!!)
    {-# INLINE idx #-}

    (!!)::a -> Indexer a -> Individual a
    (!!) = idx
    {-# INLINE (!!) #-}
    infixr 9 !!

-- | Characterizes an index that can safely fail
class SafeIx s i where
    
    lookup::s -> i -> Maybe (Individual s)
    lookup = (!?)
    {-# INLINE lookup #-}

    (!?)::s -> i -> Maybe (Individual  s)
    (!?) = lookup
    {-# INLINE (!?) #-}
    infixr 9 !?

-- | Characterizes an element indexed via type-level naturals    
class KnownNat i => NatIx i a where
    
    -- | The type of indexed value
    type NatIndexed i a

    -- | Retrieves the indexed value
    natix::a -> NatIndexed i a

-- | Characterizes a multi-level index    
class  (KnownNat i, OrdEnum a) => MultiIndexed i a where
    mix::UniTuple i (IxRange a) -> MIx i a
    milevels::MIx i a -> [UniTuple i a]

-- | Constructs an index range given a pair representing a lower/uppper bound    
ixrange::(OrdEnum a) => (a,a) -> IxRange a
ixrange = IxRange 

-- | Constructs a t-parametric term indexed by 'i'
term::i -> (i -> t) -> IxTerm i t
term i t = IxTerm (i,t)

-- | Evaluates a term
termval::IxTerm i t -> t
termval (IxTerm (i,f)) = f i

-- | Determines the value of the terms's index
termix::IxTerm i t -> i
termix (IxTerm (i,t)) = i

ixfamily::(Ord i) => [IxTerm i t] -> IxFamily i t
ixfamily terms = (\t -> (termix t, t)) <$> terms |> Map.fromList |> IxFamily

-- | Constructs a lower index
lix::a -> LIx a
lix = LIx

-- | Constructs a upper index
uix::a -> UIx a
uix = UIx

bix::[LIx a] -> [UIx a] -> BiIx a
bix l u = BiIx (l,u)

-------------------------------------------------------------------------------            
-- * Indexable class memberships
-------------------------------------------------------------------------------            
instance (Eq a) => Indexable [a] where    
    idx = (List.!!)

instance Indexable (Seq a) where
    idx = Sequence.index
    
instance Indexable (Vector a) where
    idx vector i = vector Vector.! i

instance (Ord k) => Indexable (Map k v) where
    type Indexer (Map k v) = k
    idx map k = (k, map Map.! k)


-------------------------------------------------------------------------------            
-- * SafeIx class membership
-------------------------------------------------------------------------------            

instance (Ord k) => SafeIx (Map k v) k where
    lookup map k = case (map Map.!? k) of
                    Just v -> Just (k, v)
                    _      -> Nothing
   
instance (Eq a,Integral k) =>  SafeIx [a] k where
    lookup src k = case valid of
            True -> Just (src !! (fromIntegral k))
            _    -> Nothing
        where            
            nonempty = (List.null src) == False
            upperBound = fromIntegral (sub' (List.length src) 1)
            inbounds = between' k (0, upperBound)
            valid = and' inbounds nonempty
                    
-------------------------------------------------------------------------------            
-- * IxRange class membership
-------------------------------------------------------------------------------            

instance OrdEnum i => Membership (IxRange i) where
    members (IxRange (a,b)) = [a..b]
        
instance (Formattable i) => Formattable (IxRange i) where
    format (IxRange (min,max)) 
        = fence LBrack RBrack (format min <> pad Dots <> format max)

instance (Formattable i) => Show (IxRange i) where        
    show = string . format
    
instance (OrdEnum a) => SetBuilder (IxRange a) a where    
    set (IxRange (i,j)) = finset s  where
        s = [i..j]
        count = add' (fromIntegral (List.length s)) 1
        
-------------------------------------------------------------------------------            
-- * IxTerm class membership
-------------------------------------------------------------------------------            

instance (Formattable i, Formattable t) => Formattable (IxTerm i t) where
    format (IxTerm (i,f)) = format (i, f i)


instance (Formattable i, Formattable t) => Show (IxTerm i t) where
    show = string . format
    
instance Functor (IxTerm i) where
    fmap f (IxTerm (i,t)) = IxTerm (i, f . t) where
            
instance (Eq i) => Eq (IxTerm i t) where
    (IxTerm (i,t)) == (IxTerm (j,s)) = i == j

instance (Ord i) => Ord (IxTerm i t) where
    compare (IxTerm (i,t)) (IxTerm (j,s)) = compare i j

-------------------------------------------------------------------------------            
-- * IxFamily class membership
-------------------------------------------------------------------------------            
    
instance (Ord i) => IsList (IxFamily i a) where
    type Item (IxFamily i a) = IxTerm i a
    toList (IxFamily map) = associated map
    fromList l = ixfamily l                            

instance (Ord k) => Indexable (IxFamily k v) where
    type Indexer (IxFamily k v) = k
    idx (IxFamily map) k = map Map.! k

-------------------------------------------------------------------------------            
-- * MultiIndexed instances
-------------------------------------------------------------------------------            
    
instance (OrdEnum a) => MultiIndexed 1 a where    
    mix (UniTuple1 r) =  UniTuple1 r
    milevels (UniTuple1 r) = [UniTuple1 a | a <- members r]

instance (OrdEnum a) => MultiIndexed 2 a where    
    mix (r1, r2) = (r1, r2)
    milevels (r1, r2) 
        = [(a1,a2) | a1 <- members r1, a2 <- members r2]
                    
instance (OrdEnum a) => MultiIndexed 3 a where    
    mix (r1, r2, r3) = (r1 , r2, r3)
    milevels (r1, r2, r3) 
        = [(a1,a2,a3) | a1 <- members r1, a2 <- members r2, a3 <- members r3]

instance (OrdEnum a) => MultiIndexed 4 a where    
    mix (r1, r2, r3, r4) = (r1 , r2, r3, r4)
    milevels (r1, r2, r3, r4) 
        =  [(a1,a2,a3,a4) | 
                a1 <- members r1, 
                a2 <- members r2, 
                a3 <- members r3, 
                a4 <- members r4]

instance (OrdEnum a) => MultiIndexed 5 a where    
    mix (r1, r2, r3, r4, r5) = (r1 , r2, r3, r4, r5)
    milevels (r1, r2, r3, r4, r5) 
        =  [(a1,a2,a3,a4,a5) | 
                a1 <- members r1, 
                a2 <- members r2, 
                a3 <- members r3, 
                a4 <- members r4, 
                a5 <- members r5]    