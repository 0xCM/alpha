-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Group
(
    MultiplicativeGroup(..),
    AbelianGroup(..),
    commutator,
    
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Negatable
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Reciprocative
import Alpha.Canonical.Algebra.Abelian
import Alpha.Canonical.Algebra.Monoidal

import Alpha.Canonical.Relations
import qualified Data.Monoid as Monoid

-- | Characterizes a type 'a' whose individuals are invertible via 'f'
class (Structure a, Inverter f) => Invertible a f where
    inverter::a -> f
    
    
-- | A multiplicative group, not necessarily commutive
class (Monoidal a, Reciprocative a, Structure a) => MultiplicativeGroup a where
    
-- | An additive group, always commutative
class (Abelian a, Negatable a, Structure a) => AbelianGroup a where   
    

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Reciprocative a) => O2 a -> O2 a
commutator o =  \x y ->  o (o (reciprocal x) (reciprocal y)) (o x y) where


instance Eq a => Structure (MultiplicativeGroup a) where
    type Individual (MultiplicativeGroup a) = a
    
instance Eq a => Structure (AbelianGroup a) where
    type Individual (AbelianGroup a) = a

instance (Integral a) => MultiplicativeGroup (Ratio a) where
instance MultiplicativeGroup Float where 
instance MultiplicativeGroup Double where 
instance MultiplicativeGroup CFloat where 
instance MultiplicativeGroup CDouble where 
    
instance AbelianGroup Integer where 
instance AbelianGroup Int where 
instance AbelianGroup Int8 where 
instance AbelianGroup Int16 where 
instance AbelianGroup Int32 where 
instance AbelianGroup Int64 where     
instance (Integral a) => AbelianGroup (Ratio a) where 
instance AbelianGroup Float where 
instance AbelianGroup Double where 
instance AbelianGroup CFloat where 
instance AbelianGroup CDouble where         
instance AbelianGroup Natural where 
instance AbelianGroup Word where 
instance AbelianGroup Word8 where 
instance AbelianGroup Word16 where 
instance AbelianGroup Word32 where 
instance AbelianGroup Word64 where     
        