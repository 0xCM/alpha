module Alpha.Canonical.Algebra.Ring
(
    module X,
    Ring(..),
    CommutativeRing(..),
    UnitalRing(..),

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Distributive as X
import Alpha.Canonical.Algebra.Divisive as X
import Alpha.Canonical.Algebra.Group as X
import Alpha.Canonical.Algebra.Semiring as X
import Alpha.Canonical.Algebra.Unsigned as X

import qualified Data.List as List


-- | A ring (with identity)
-- See https://en.wikipedia.org/wiki/Ring_(mathematics)     
class (AbelianGroup a, Monoidal a, Distributive a, Absolute a)
    => Ring a where
        
-- | A ring in which the the mulplication operation is also commutative
class Ring a => CommutativeRing a where

-- | A unital ring is a ring with a multiplicative identity element
-- See Y2018MTLA   
class (Unital a, Ring a) => UnitalRing a where

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
    

instance CommutativeRing Integer    
instance CommutativeRing Int
instance CommutativeRing Int8
instance CommutativeRing Int16
instance CommutativeRing Int32
instance CommutativeRing Int64
instance (Integral a) => CommutativeRing (Ratio a)
instance CommutativeRing Natural where 
instance CommutativeRing Word where 
instance CommutativeRing Word8 where 
instance CommutativeRing Word16 where 
instance CommutativeRing Word32 where 
instance CommutativeRing Word64 where             
    
