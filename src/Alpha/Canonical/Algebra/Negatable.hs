-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Negatable
(    
    Negatable(..),
    Negation(..), 
    Binegatable(..),
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Subtractive as X
import qualified Data.List as List

-- | Characterizes types whose values are closed under 
-- additive negation
class Negatable a where
    -- | Negates the operand    
    negate::a -> a

    -- | Accepts a list of negatable things, creates
    -- a new list by negating each element in the
    -- input list and returns the admixture vis-a-vis
    -- 'intermix'
    alternate::[a] -> [a]
    alternate x = intermix x (negate <$> x)

-- / Characterizes types for which unary negation is defined
class Binegatable a where
    type Negated a
    -- | Negates the operand    
    binegate::a -> Negated a

-- | Represents the negation of the encapsulated content
newtype Negation a = Negation a
    deriving(Eq,Ord,Generic,Data,Typeable)
instance Newtype (Negation a)

-- | Constructs a negation computation
negation::(Negatable a) => a -> Negation a
negation = Negation

-- *Computable instances
-------------------------------------------------------------------------------
instance (Negatable a) => Computable (Negation a) where
    type Computed (Negation a) = a
    compute (Negation a) = negate a    
         
-- *Negatable instances
-------------------------------------------------------------------------------
instance Negatable a => Negatable (Vector a) where
    negate v = negate <$> v 

instance Negatable a => Negatable [a] where
    negate src = negate <$> src
    
instance Negatable Integer where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int8 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int16 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int32 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int64 where 
    negate = negate'
    {-# INLINE negate #-}
instance (Integral a) => Negatable (Ratio a) where 
    negate x = sub 0 x
    {-# INLINE negate #-}    
instance Negatable Float where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Double where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CFloat where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CDouble where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Natural where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word8 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word16 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word32 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word64 where 
    negate x = sub 0 x
    {-# INLINE negate #-}

-- *Negatable tuples
-------------------------------------------------------------------------------
type Negatable2 a1 a2 = (Negatable a1, Negatable a2)
type Negatable3 a1 a2 a3 = (Negatable2 a1 a2, Negatable a3)
type Negatable4 a1 a2 a3 a4 = (Negatable3 a1 a2 a3, Negatable a4)
type Negatable5 a1 a2 a3 a4 a5 = (Negatable4 a1 a2 a3 a4, Negatable a5)

instance Negatable a => Negatable (UniTuple1 a) where
    negate (UniTuple1 x) = UniTuple1 (negate x)

instance Negatable2 a1 a2 => Negatable (Tuple2 a1 a2) where    
    negate (a1,a2) = (negate a1, negate a2)

instance Negatable3 a1 a2 a3 => Negatable (Tuple3 a1 a2 a3) where
    negate (a1,a2,a3) = (negate a1, negate a2, negate a3)

instance Negatable4 a1 a2 a3 a4 => Negatable (Tuple4 a1 a2 a3 a4) where
    negate (a1,a2,a3,a4) = (negate a1, negate a2, negate a3, negate a4)

instance Negatable5 a1 a2 a3 a4 a5  => Negatable (Tuple5 a1 a2 a3 a4 a5)  where
    negate (a1,a2,a3,a4,a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)                    