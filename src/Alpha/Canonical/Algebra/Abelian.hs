-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Abelian
(
    Abelian(..)        
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive

-- | A (commutative) additive structure with an identity element
-- In other words, 'Monoidal' with zero and commutative addition
-- instead of one and multiplication
class (Nullary a, Additive a) => Abelian a where


instance Abelian Integer where 
instance Abelian Int where 
instance Abelian Int8 where 
instance Abelian Int16 where 
instance Abelian Int32 where 
instance Abelian Int64 where     
instance Abelian Natural where 
instance Abelian Word where 
instance Abelian Word8 where 
instance Abelian Word16 where 
instance Abelian Word32 where 
instance Abelian Word64 where             
instance (Integral a) => Abelian (Ratio a) where 
instance Abelian Float where 
instance Abelian Double where 
instance Abelian CFloat where 
instance Abelian CDouble where 
        

