-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Group
(
    module X,
    MultiplicativeGroup(..),
    AbelianGroup(..),
    commutator
    
    
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Additive as X
import Alpha.Canonical.Algebra.Subtractive as X
import Alpha.Canonical.Algebra.Negatable as X
import Alpha.Canonical.Algebra.Multiplicative as X
import Alpha.Canonical.Algebra.Reciprocative as X
import Alpha.Canonical.Algebra.Abelian as X
import Alpha.Canonical.Algebra.Monoidal as X

import Alpha.Canonical.Relations
import qualified Data.Monoid as Monoid

        
-- | A multiplicative group, not necessarily commutive
class (Monoidal a, Reciprocative a) => MultiplicativeGroup a where
    
-- | An additive group, always commutative
class (Abelian a, Negatable a) => AbelianGroup a where   

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Inverter f a, BinaryOperator g a) => g -> f -> O2 a
commutator g f =  \x y ->  o (o (i x) (i y)) (o x y) where
    o = o2 g
    i = inverter f
    


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
        
