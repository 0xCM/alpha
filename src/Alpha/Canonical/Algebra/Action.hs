-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Action
(    
    LeftAction(..), RightAction(..), 
    
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive

-- | Characterizes a left algebraic/group action
class LeftAction k a where
    -- | Effects a left action by k on a
    leftmul::k -> a -> a
    leftmul = (*.)

    -- | Effects a left action by k on a
    (*.)::k -> a -> a
    (*.) = leftmul
    infixl 5 *.

instance (Additive a, Nullary a) => LeftAction Integer a where
    leftmul = nsum
    
-- | Characterizes a right algebraic/group action    
class RightAction a k where

    -- | Effects a right action by k on a
    rightmul::a -> k -> a

    -- | Right-multiplication of a by a Action k    
    (.*)::a -> k -> a
    (.*) = rightmul
    infixl 5 .*

instance (Additive a, Nullary a) => RightAction a Integer where

    rightmul a k = nsum k a
    
-- | Represents a left-linear combination of elements
newtype LeftSum k a = LeftSum [(k,a)]

-- | Represents a right-linear combination of elements
newtype RightSum a k = RightSum [(a,k)]

instance (LeftAction k a) => Computable (LeftSum k a) where
    type Computed (LeftSum k a) = MultiSum a
    
    compute (LeftSum pairs) = scale <$> pairs |> multisum where
        scale::(k,a) -> a
        scale (k,a) = k *. a

instance (RightAction a k) => Computable (RightSum a k) where
    type Computed (RightSum a k) = MultiSum a
    
    compute (RightSum pairs) = scale <$> pairs |> multisum   where
        scale::(a,k) -> a
        scale (a,k) = a .* k
                
-- LeftAction 
-------------------------------------------------------------------------------

instance LeftAction Natural Natural where
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int Int where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int8 Int8 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int16 Int16 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int32 Int32 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int64 Int64 where 
    leftmul = mul'    
    {-# INLINE leftmul #-}

instance LeftAction Word Word where 
    leftmul = mul'    
    {-# INLINE leftmul #-}

instance LeftAction Word8 Word8 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word16 Word16 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word32 Word32 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word64 Word64 where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Float Float where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Double Double where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction CFloat CFloat where 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction CDouble CDouble where 
    leftmul = mul'
    {-# INLINE leftmul #-}

-- RightAction 
-------------------------------------------------------------------------------

instance RightAction Natural Natural where
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int Int where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int8 Int8 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int16 Int16 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int32 Int32 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int64 Int64 where 
    rightmul = mul'    
    {-# INLINE rightmul #-}

instance RightAction Word Word where 
    rightmul = mul'    
    {-# INLINE rightmul #-}

instance RightAction Word8 Word8 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word16 Word16 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word32 Word32 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word64 Word64 where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Float Float where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Double Double where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction CFloat CFloat where 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction CDouble CDouble where 
    rightmul = mul'
    {-# INLINE rightmul #-}    

    
instance LeftAction k a => LeftAction k (UniTuple2 a) where
    leftmul k (x1,x2) = (k *. x1, k *. x2)
    
instance LeftAction k a => LeftAction k (UniTuple3 a) where

    leftmul k (x1,x2,x3) = (k *. x1, k *. x2, k *. x3)
    
instance LeftAction k a => LeftAction k (UniTuple4 a) where

    leftmul k (x1,x2,x3,x4) = (k *. x1, k *. x2, k *. x3, k *. x4)
    
instance LeftAction k a => LeftAction k (UniTuple5 a) where

    leftmul k (x1,x2,x3,x4,x5) = (k *. x1, k *. x2, k *. x3, k *. x4, k *. x5)
    