{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Relations.Comparison
(        
    Extremum(..),
    Infimum(..), Infimal(..),
    Supremum(..), Supremal(..),
    Minimal(..), Maximal(..),

    Comparer(..),
    LT(..), GT(..), EQ(..), LTEQ(..), GTEQ(..), Comparable(..),
    ULT(..), UGT(..), ULTEQ(..), UGTEQ(..), UniformlyComparable(..),


) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Element
import qualified Numeric.Interval as Interval
import qualified Prelude as P

type family Infimum a
type family Supremum a    
type family Extremum a

type instance Extremum (Interval a) = a

-- | Synonym for function that effects heterogenous comparison
type Comparer a b = a -> b -> Bool


-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is minimal in A if a <= x for all x in A
class Minimal a where
    -- A minimal element 
    minimum::a -> Element a

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is maximal in A if a >= x for all x in A
class Maximal a where
    maximum::a -> Element a
    
-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimal a where
    -- / The greatest lower bound
    infimum::a -> Extremum a
    
-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremal a where
    -- / The least upper bound
    supremum::a -> Extremum a

-- / Characterizes pairs of (potentially) heterogenous types that
-- support a less then relation
class LT a b where
    lt::Comparer a b

    (.<.)::Comparer a b
    (.<.) = lt
    {-# INLINE (.<.) #-}
    infix 4 .<.

-- / Characterizes pairs of (potentially) heterogenous types that
-- support a greater than relation
class GT a b where
    gt::Comparer a b

    (.>.)::Comparer a b
    (.>.) = gt
    {-# INLINE (.>.) #-}
    infix 4 .>.

class EQ a b where
    eq::Comparer a b
    
    (>==<)::Comparer a b
    (>==<) = eq
    {-# INLINE (>==<) #-}
    infix 4 >==<

class LTEQ a b where
    lteq::Comparer a b

    (.<=.)::Comparer a b
    (.<=.) = lteq
    {-# INLINE (.<=.) #-}
    infix 4 .<=.

class GTEQ a b where
    gteq::Comparer a b

    (.>=.)::Comparer a b
    (.>=.) = gteq
    {-# INLINE (.>=.) #-}
    infix 4 .>=.

class (LT a b, GT a b, EQ a b, LTEQ a b, GTEQ a b) => Comparable a b where

class (Ord a) => ULTEQ a where
    (<=)::Comparer a a
    (<=) a b= a P.<= b
    infix 4 <=
    {-# INLINE (<=) #-}

    -- Computes the minimum of two values    
    min::a -> a -> a
    min x y = ifelse (x <= y) x y
    {-# INLINE min #-}

class (Ord a) => ULT a where
    (<)::Comparer a a
    (<) a b = a P.< b
    infix 4 <    
    {-# INLINE (<) #-}

class (Ord a) => UGT a where    
    (>)::Comparer a a
    (>) a b = a P.> b
    infix 4 >
    {-# INLINE (>) #-}

class (Ord a) => UGTEQ a where
    (>=)::Comparer a a
    (>=) a b = a P.>= b
    infix 4 >=
    {-# INLINE (>=) #-}

    -- Computes the maximum of two values
    max::(Ord a) => a -> a -> a
    max x y = ifelse (x >= y) x y
    {-# INLINE max #-}

class (UGTEQ a, UGT a, ULTEQ a, ULT a) => UniformlyComparable a where            
    between::TernaryPredicate a
    between x a b = x >= a || x <= b
    {-# INLINE between #-}

-- | Encodes that values of orderable types can be related via (<=)
instance (Ord a) => ULTEQ a

-- | Encodes that values of orderable types can be related via (<)
instance (Ord a) => ULT a    

-- | Encodes that values of orderable types can be related via (>)
instance (Ord a) => UGT a    

-- | Encodes that values of orderable types can be related via (>=)
instance (Ord a) => UGTEQ a    

-- | Encodes that values of orderable types are uniformly comparable
instance (Ord a) => UniformlyComparable a

instance (Ord a, Semigroup a) => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance (Ord a, Semigroup a) => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup            