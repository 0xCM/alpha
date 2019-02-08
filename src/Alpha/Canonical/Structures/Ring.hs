-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Structures.Ring
(
    module X,
    Ring(..),
    OrderedRing(..),
    CommutativeRing(..),
    UnitalRing(..),
    DivisionRing(..),

) where
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Structures.Structure as X
import Alpha.Canonical.Structures.Group as X

import qualified Data.List as List


-- | A ring (with identity)
-- See https://en.wikipedia.org/wiki/Ring_(mathematics)     
class (AbelianGroup a, ProductMonoid a, Distributive a) => Ring a where
                
-- | A ring in which the the mulplication operation is also commutative
--class Ring a => CommutativeRing a where
type CommutativeRing a = (Commutative a, Ring a)

-- | A ring with a total order
-- See https://en.wikipedia.org/wiki/Ordered_ring
type OrderedRing a = (Ord a, Ring a) 

-- | A unital ring is a ring with a multiplicative identity element
type UnitalRing a = (Unital a, Ring a)

-- | A unital ring in which every nonzero element ahs a multiplicative inverse
-- See https://en.wikipedia.org/wiki/Division_ring    
type DivisionRing a = (UnitalRing a, Invertible a, Divisive a)
    
instance Ring Integer    
instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance (Integral a) => Ring (Ratio a)
instance Ring Natural where 
instance Ring Word where 
instance Ring Word8 where 
instance Ring Word16 where 
instance Ring Word32 where 
instance Ring Word64 where             
instance Ring Float where 
instance Ring Double where 
instance Ring CFloat where 
instance Ring CDouble where
instance Ring a => Ring (ComplexNumber a)