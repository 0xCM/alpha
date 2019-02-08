-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Common.Indexing
(
    module X,
    NatIx(..),
    SafeIx(..),
    IxTerm(..), 
    IxRange(..),    
    IxFamily(..),
    UpperIx(..),
    LowerIx(..),
    MultiIx(..),
    NestedTerm(..),
    lowerix,
    upperix,
    multix,
    term,
    termix,
    termvalue,
    ixrange,
    rangedterm,
    nestedrange,
    nestedterm

) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual as X
import Alpha.Canonical.Common.Format as X
import Alpha.Canonical.Common.Conversions as X

import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream
import qualified Data.Sequence as Sequence

-- | Represents a lower index
newtype LowerIx a = LowerIx a
    deriving (Eq, Ord, Enum, Generic, Data, Typeable)

-- | Represents an upper index
newtype UpperIx a = UpperIx a
    deriving (Eq, Ord, Enum, Generic, Data, Typeable)

-- | Represents a bipartite index consisting of an arbitrary
-- number of lower and upper indexes
newtype MultiIx a = MultiIx (Vector (LowerIx a), Vector (UpperIx a))
    deriving (Eq, Ord, Generic, Data, Typeable)
        
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
    deriving (Eq,Ord,Generic,Data,Functor,Typeable)
instance Newtype (IxRange i)    

type instance Individual (IxFamily i t) = IxTerm i t
type instance Individual (IxRange i) = i

-- | Defines a term together with the range that will be used
-- upon expansion
newtype RangedTerm i t = RangedTerm (IxRange i, IxTerm i t)
    deriving (Generic)
instance Newtype (RangedTerm i t)

newtype NestedRange i = NestedRange (IxRange i, IxRange i)
    deriving (Eq,Ord,Generic,Data,Functor,Typeable)

newtype NestedTerm i t = NestedTerm 
    (
        IxRange i,      -- The outer range
        IxRange i,       -- The inner range
        IxTerm (i,i) t  -- The inner term
    )
    deriving (Generic)
instance Newtype (NestedTerm i t)      

        
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

-- | Associates an individual with an index value
newtype Indexed a = Indexed (Indexer a, Individual a)
    deriving (Generic)
instance Newtype (Indexed a)

-- | Constructs an index range given a pair representing a lower/uppper bound    
ixrange::(OrdEnum a) => (a,a) -> IxRange a
ixrange = IxRange 

-- | Constructs a t-parametric term indexed by 'i'
term::i -> (i -> t) -> IxTerm i t
term i t = IxTerm (i,t)

rangedterm::IxRange i -> IxTerm i t -> RangedTerm i t
rangedterm range term = RangedTerm (range, term)

nestedterm::(OrdEnum i) => (i,i) -> (i,i) -> IxTerm (i,i) t -> NestedTerm i t
nestedterm or ir term  = NestedTerm (ixrange or, ixrange ir,term)

nestedrange::(OrdEnum i) => (i,i) -> (i,i) -> NestedRange i
nestedrange or ir = NestedRange (ixrange or, ixrange ir)

-- | Evaluates a term
termvalue::IxTerm i t -> t
termvalue (IxTerm (i,f)) = f i

-- | Determines the value of the terms's index
termix::IxTerm i t -> i
termix (IxTerm (i,t)) = i

ixfamily::(Ord i) => [IxTerm i t] -> IxFamily i t
ixfamily terms = (\t -> (termix t, t)) <$> terms |> Map.fromList |> IxFamily

-- | Constructs a lower index
lowerix::a -> LowerIx a
lowerix = LowerIx

-- | Constructs a upper index
upperix::a -> UpperIx a
upperix = UpperIx

multix::Vector(LowerIx a) -> Vector(UpperIx a) -> MultiIx a
multix l u = MultiIx (l,u)

-- | Constructs  an indexed value
indexed::Indexable a => Indexer a -> Individual a -> Indexed a
indexed i ind = Indexed (i, ind)


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

instance OrdEnum i => Discrete (IxRange i) where
    individuals (IxRange (a,b)) = [a..b]
        
instance (Formattable i) => Formattable (IxRange i) where
    format (IxRange (min,max)) 
        = fence LBrack RBrack (format min <> pad Dots <> format max)

instance (Formattable i) => Show (IxRange i) where        
    show = string . format
            
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

-- * IxFamily class membership
-------------------------------------------------------------------------------                
instance (Ord i) => IsList (IxFamily i a) where
    type Item (IxFamily i a) = IxTerm i a
    toList (IxFamily map) = associated map
    fromList l = ixfamily l                            

instance (Ord k) => Indexable (IxFamily k v) where
    type Indexer (IxFamily k v) = k
    idx (IxFamily map) k = map Map.! k

-- * NestedRange membership
-------------------------------------------------------------------------------            
instance (Formattable i) => Formattable (NestedRange i) where
    format (NestedRange r) = format r

instance (Formattable i) => Show (NestedRange i) where        
    show = string . format

instance (OrdEnum i) => Expansive (NestedRange i) where
    type Expanded (NestedRange i) = (i,i)

    expand (NestedRange (outer,inner)) = do
        o <- individuals outer
        i <- individuals inner
        return (o,i)
    
-- * NestedTerm membership
-------------------------------------------------------------------------------            
instance (OrdEnum i) => Expansive (NestedTerm i t) where
    type Expanded (NestedTerm i t) = t

    expand (NestedTerm (or, ir, (IxTerm (_, f ) ))) 
        = [termvalue (term (i,j) f)  | i <- individuals or, j <- individuals ir]
    