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
import qualified Data.List as List

-- | A (commutative) additive structure with an identity element
-- In other words, 'Monoidal' with zero and commutative addition
-- instead of one and multiplication
class (Nullary a, Additive a) => Abelian a where
    -- | Calculates the sum of 'n' copies of 'a'
    nsum::(Integral n) => n -> a -> a
    nsum n a = reduce zero (+) (clone n a)



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
        
type Abelian2 a1 a2 = (Abelian a1, Abelian a2)
type Abelian3 a1 a2 a3 = (Abelian2 a1 a2, Abelian a3)
type Abelian4 a1 a2 a3 a4 = (Abelian3 a1 a2 a3, Abelian a4)
type Abelian5 a1 a2 a3 a4 a5 = (Abelian4 a1 a2 a3 a4, Abelian a5)

instance Abelian2 a1 a2 => Abelian (Tuple2 a1 a2)
instance Abelian3 a1 a2 a3 => Abelian (Tuple3 a1 a2 a3)
instance Abelian4 a1 a2 a3 a4 => Abelian (Tuple4 a1 a2 a3 a4)
instance Abelian5 a1 a2 a3 a4 a5 => Abelian (Tuple5 a1 a2 a3 a4 a5)
