module Alpha.Canonical.Algebra.Comparable
(
    Comparer(..),
    IsLT(..),
    IsGT(..), 
    IsEQ(..), 
    IsLTEQ(..), 
    IsGTEQ(..), 
    Comparable(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators


class IsLT a b where
    lt::Comparer a b

    (.<.)::Comparer a b
    (.<.) = lt
    {-# INLINE (.<.) #-}
    infix 4 .<.

class IsGT a b where
    gt::Comparer a b

    (.>.)::Comparer a b
    (.>.) = gt
    {-# INLINE (.>.) #-}
    infix 4 .>.

class IsEQ a b where
    eq::Comparer a b
    
    (>==<)::Comparer a b
    (>==<) = eq
    {-# INLINE (>==<) #-}
    infix 4 >==<

class IsLTEQ a b where
    lteq::Comparer a b

    (.<=.)::Comparer a b
    (.<=.) = lteq
    {-# INLINE (.<=.) #-}
    infix 4 .<=.

class IsGTEQ a b where
    gteq::Comparer a b

    (.>=.)::Comparer a b
    (.>=.) = gteq
    {-# INLINE (.>=.) #-}
    infix 4 .>=.

class (IsLT a b, IsGT a b, IsEQ a b,  IsLTEQ a b, IsGTEQ a b) => Comparable a b where

