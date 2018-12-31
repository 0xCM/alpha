-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Algebra.Group
(
    module X,
    MultiplicativeGroup(..),
    AbelianGroup(..),
    FiniteAbelianGroup(..),
    commutator,
    abs
    
    
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Additive as X
import Alpha.Canonical.Algebra.Subtractive as X
import Alpha.Canonical.Algebra.Negatable as X
import Alpha.Canonical.Algebra.Multiplicative as X
import Alpha.Canonical.Algebra.Reciprocative as X
import Alpha.Canonical.Algebra.Monoidal as X

import Alpha.Canonical.Relations
import qualified Data.Monoid as Monoid

class (Compositer a, Inverter a, Identity a) => Group a where
        
-- | A multiplicative group, not necessarily commutive
class (Monoidal a, Reciprocative a) => MultiplicativeGroup a where
    
-- | An additive group, always commutative
class (Eq a, Nullary a, Additive a, Negatable a) => AbelianGroup a where  
 
class (AbelianGroup a, Finite a) => FiniteAbelianGroup a where

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::forall g a . (Inverter a) => O2 a -> O2 a
commutator o  =  \x y ->  o (o (i x) (i y)) (o x y) where
    i = inverter @a
 
abs::(Nullary a, Ord a, Negatable a) => a -> a
abs a = ifelse (a >= zero) a (negate a)
    
instance Integral a => MultiplicativeGroup (Ratio a) where
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
instance AbelianGroup Natural where 
instance AbelianGroup Word where 
instance AbelianGroup Word8 where 
instance AbelianGroup Word16 where 
instance AbelianGroup Word32 where 
instance AbelianGroup Word64 where     
instance (Integral a) => AbelianGroup (Ratio a) where 
instance AbelianGroup Float where 
instance AbelianGroup Double where 
instance AbelianGroup CFloat where 
instance AbelianGroup CDouble where                         
        

type AG2 a1 a2 = (AbelianGroup a1, AbelianGroup a2)
type AG3 a1 a2 a3 = (AG2 a1 a2, AbelianGroup a3)
type AG4 a1 a2 a3 a4 = (AG3 a1 a2 a3, AbelianGroup a4)
type AG5 a1 a2 a3 a4 a5 = (AG4 a1 a2 a3 a4, AbelianGroup a5)

instance AG2 a1 a2 => AbelianGroup (Tuple2 a1 a2)
instance AG3 a1 a2 a3 => AbelianGroup (Tuple3 a1 a2 a3)
instance AG4 a1 a2 a3 a4 => AbelianGroup (Tuple4 a1 a2 a3 a4)
instance AG5 a1 a2 a3 a4 a5 => AbelianGroup (Tuple5 a1 a2 a3 a4 a5)