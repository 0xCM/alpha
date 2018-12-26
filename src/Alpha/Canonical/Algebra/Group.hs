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

class (Structure (Group a) a, Compositer a, Inverter a, Identity a) => Group a where
        
-- | A multiplicative group, not necessarily commutive
class (Structure (MultiplicativeGroup a) a, Monoidal a, Reciprocative a) => MultiplicativeGroup a where
    
-- | An additive group, always commutative
class (Abelian a, Negatable a) => AbelianGroup a where   

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::forall g a . (Inverter a, BinaryOperator g a) => g -> O2 a
commutator g  =  \x y ->  o (o (i x) (i y)) (o x y) where
    o = o2 g
    i = inverter @a
    
    
instance Integral a => MultiplicativeGroup (Ratio a) where
instance MultiplicativeGroup Float where 
instance MultiplicativeGroup Double where 
instance MultiplicativeGroup CFloat where 
instance MultiplicativeGroup CDouble where 

instance Integral a => Structure (MultiplicativeGroup (Ratio a)) (Ratio a)
instance Structure (MultiplicativeGroup Float) Float
instance Structure (MultiplicativeGroup Double) Double
instance Structure (MultiplicativeGroup CFloat) CFloat
instance Structure (MultiplicativeGroup CDouble) CDouble

instance Integral a => DiscreteStructure (MultiplicativeGroup (Ratio a)) (Ratio a)


instance Structure (AbelianGroup Integer) Integer
instance Structure (AbelianGroup Int) Int
instance Structure (AbelianGroup Int8) Int8
instance Structure (AbelianGroup Int16) Int16
instance Structure (AbelianGroup Int32) Int32
instance Structure (AbelianGroup Int64) Int64    
instance Structure (AbelianGroup Natural) Natural
instance Structure (AbelianGroup Word) Word
instance Structure (AbelianGroup Word8) Word8
instance Structure (AbelianGroup Word16) Word16
instance Structure (AbelianGroup Word32) Word32
instance Structure (AbelianGroup Word64) Word64    
instance (Integral a) => Structure (AbelianGroup (Ratio a) ) (Ratio a)
instance Structure (AbelianGroup Float) Float 
instance Structure (AbelianGroup Double) Double where 
instance Structure (AbelianGroup CFloat) CFloat where 
instance Structure (AbelianGroup CDouble) CDouble where         
    
instance DiscreteStructure (AbelianGroup Integer) Integer
instance DiscreteStructure (AbelianGroup Int) Int
instance DiscreteStructure (AbelianGroup Int8) Int8
instance DiscreteStructure (AbelianGroup Int16) Int16
instance DiscreteStructure (AbelianGroup Int32) Int32
instance DiscreteStructure (AbelianGroup Int64) Int64    
instance DiscreteStructure (AbelianGroup Natural) Natural
instance DiscreteStructure (AbelianGroup Word) Word
instance DiscreteStructure (AbelianGroup Word8) Word8
instance DiscreteStructure (AbelianGroup Word16) Word16
instance DiscreteStructure (AbelianGroup Word32) Word32
instance DiscreteStructure (AbelianGroup Word64) Word64    
instance (Integral a) => DiscreteStructure (AbelianGroup (Ratio a) ) (Ratio a)
    
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