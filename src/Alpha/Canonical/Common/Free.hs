-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Free
(
    module X,    
    FreeSum(..),
    freesum,
) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Format as X
import Alpha.Canonical.Common.Conversions as X
import Alpha.Canonical.Common.Individual as X

-- Represents a formal sum of all distinct pairs (a,b)
-- See https://en.wikipedia.org/wiki/Linear_combination
newtype FreeSum a b = FreeSum [(a,b)]
    deriving (Eq,Ord,Generic,Data,Typeable,Groupable)

type instance Individual (FreeSum a b) = (a,b)

-- | Constructs a (maximal) free sum of basis elements over a set of coefficents
freesum::[a] -> [b] -> FreeSum a b
freesum coeff basis = FreeSum $ do
    v <- basis
    s <- coeff
    return (s, v)
    
instance (Formattable a, Formattable b) => Formattable (FreeSum a b) where
    format (FreeSum pairs) = splat (weave (pad Plus) parts)  where
        parts = do
            (s,v) <- pairs
            return (splat [format s , format v] )
    
instance (Formattable a, Formattable b) => Show (FreeSum a b) where
    show =  string . format

-- instance (Eq a,Eq b) => Grouping (FreeSum a b) where
--     groups (\(a,b) (c,d) -> b == d) s