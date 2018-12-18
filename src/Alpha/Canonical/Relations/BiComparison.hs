module Alpha.Canonical.Relations.BiComparison
(        
    BiLT(..), 
    BiGT(..), 
    BiEQ(..), 
    BiLTEQ(..), 
    BiGTEQ(..), 
    BiComparable(..), 
    BiComparer(..),

) where
import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Relations.Related

import qualified Numeric.Interval as Interval
import qualified Prelude as P


-- | Synonym for function that effects heterogenous comparison
type BiComparer a b = a -> b -> Bool


-- / Characterizes pairs of (potentially) heterogenous types that
-- support a less then relation
class BiLT a b where
    lt::BiComparer a b

    (.<.)::BiComparer a b
    (.<.) = lt
    {-# INLINE (.<.) #-}
    infix 4 .<.

-- / Characterizes pairs of (potentially) heterogenous types that
-- support a greater than relation
class BiGT a b where
    gt::BiComparer a b

    (.>.)::BiComparer a b
    (.>.) = gt
    {-# INLINE (.>.) #-}
    infix 4 .>.

class BiEQ a b where
    eq::BiComparer a b
    
    (>==<)::BiComparer a b
    (>==<) = eq
    {-# INLINE (>==<) #-}
    infix 4 >==<

class BiLTEQ a b where
    lteq::BiComparer a b

    (.<=.)::BiComparer a b
    (.<=.) = lteq
    {-# INLINE (.<=.) #-}
    infix 4 .<=.

class BiGTEQ a b where
    gteq::BiComparer a b

    (.>=.)::BiComparer a b
    (.>=.) = gteq
    {-# INLINE (.>=.) #-}
    infix 4 .>=.

class (BiLT a b, BiGT a b, BiEQ a b, BiLTEQ a b, BiGTEQ a b) => BiComparable a b where


