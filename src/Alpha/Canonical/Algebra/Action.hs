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

class LeftAction k a where
    type LeftProduct k a
    type LeftProduct k a = a

    -- | Effects a left action by k on a
    leftmul::k -> a -> LeftProduct k a
    leftmul = (*.)

    -- | Effects a left action by k on a
    (*.)::k -> a -> LeftProduct k a
    (*.) = leftmul
    infixl 5 *.

class RightAction a k where
    type RightProduct a k
    type RightProduct a k = a

    -- | Effects a right action by k on a
    rightmul::a -> k -> RightProduct a k

    -- | Right-multiplication of a by a Action k    
    (.*)::a -> k -> RightProduct a k
    (.*) = rightmul
    infixl 5 .*



-- | Represents a left-linear combination of elements
newtype LeftSum k a = LeftSum [(k,a)]

-- | Represents a right-linear combination of elements
newtype RightSum a k = RightSum [(a,k)]

instance (LeftAction k a) => Computable (LeftSum k a) where
    type Computed (LeftSum k a) = Summation (LeftProduct k a)
    
    compute (LeftSum pairs) = scale <$> pairs |> summation where
        scale::(k,a) -> LeftProduct k a
        scale (k,a) = k *. a

instance (RightAction a k) => Computable (RightSum a k) where
    type Computed (RightSum a k) = Summation (RightProduct a k)
    
    compute (RightSum pairs) = scale <$> pairs |> summation   where
        scale::(a,k) -> RightProduct a k
        scale (a,k) = a .* k
                
-- LeftAction 
-------------------------------------------------------------------------------

instance LeftAction Natural Natural where
    type LeftProduct Natural Natural = Natural 
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Integer Integer where 
    type LeftProduct Integer Integer = Integer
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int Int where 
    type LeftProduct Int Int = Int
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int8 Int8 where 
    type LeftProduct Int8 Int8 = Int8
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int16 Int16 where 
    type LeftProduct Int16 Int16 = Int16
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int32 Int32 where 
    type LeftProduct Int32 Int32 = Int32
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Int64 Int64 where 
    type LeftProduct Int64 Int64 = Int64
    leftmul = mul'    
    {-# INLINE leftmul #-}

instance LeftAction Word Word where 
    type LeftProduct Word Word  = Word
    leftmul = mul'    
    {-# INLINE leftmul #-}

instance LeftAction Word8 Word8 where 
    type LeftProduct Word8 Word8 = Word8
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word16 Word16 where 
    type LeftProduct Word16 Word16 = Word16
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word32 Word32 where 
    type LeftProduct Word32 Word32 = Word32    
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Word64 Word64 where 
    type LeftProduct Word64 Word64 = Word64
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Float Float where 
    type LeftProduct Float Float = Float    
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction Double Double where 
    type LeftProduct Double Double = Double
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction CFloat CFloat where 
    type LeftProduct CFloat CFloat = CFloat    
    leftmul = mul'
    {-# INLINE leftmul #-}

instance LeftAction CDouble CDouble where 
    type LeftProduct CDouble CDouble = CDouble    
    leftmul = mul'
    {-# INLINE leftmul #-}

-- RightAction 
-------------------------------------------------------------------------------

instance RightAction Natural Natural where
    type RightProduct Natural Natural = Natural 
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Integer Integer where 
    type RightProduct Integer Integer = Integer
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int Int where 
    type RightProduct Int Int = Int
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int8 Int8 where 
    type RightProduct Int8 Int8 = Int8
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int16 Int16 where 
    type RightProduct Int16 Int16 = Int16
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int32 Int32 where 
    type RightProduct Int32 Int32 = Int32
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Int64 Int64 where 
    type RightProduct Int64 Int64 = Int64
    rightmul = mul'    
    {-# INLINE rightmul #-}

instance RightAction Word Word where 
    type RightProduct Word Word  = Word
    rightmul = mul'    
    {-# INLINE rightmul #-}

instance RightAction Word8 Word8 where 
    type RightProduct Word8 Word8 = Word8
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word16 Word16 where 
    type RightProduct Word16 Word16 = Word16
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word32 Word32 where 
    type RightProduct Word32 Word32 = Word32    
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Word64 Word64 where 
    type RightProduct Word64 Word64 = Word64
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Float Float where 
    type RightProduct Float Float = Float    
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction Double Double where 
    type RightProduct Double Double = Double
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction CFloat CFloat where 
    type RightProduct CFloat CFloat = CFloat    
    rightmul = mul'
    {-# INLINE rightmul #-}

instance RightAction CDouble CDouble where 
    type RightProduct CDouble CDouble = CDouble    
    rightmul = mul'
    {-# INLINE rightmul #-}    

instance LeftAction k a => LeftAction k (UniTuple1 a) where
    type LeftProduct k (UniTuple1 a) = LeftProduct k a

    leftmul k x = k *. x
    
instance LeftAction k a => LeftAction k (UniTuple2 a) where
    type LeftProduct k (UniTuple2 a) = UniTuple2 (LeftProduct k a)

    leftmul k (x1,x2) = (k *. x1, k *. x2)
    
instance LeftAction k a => LeftAction k (UniTuple3 a) where
    type LeftProduct k (UniTuple3 a) = UniTuple3 (LeftProduct k a)

    leftmul k (x1,x2,x3) = (k *. x1, k *. x2, k *. x3)
    
instance LeftAction k a => LeftAction k (UniTuple4 a) where
    type LeftProduct k (UniTuple4 a) = UniTuple4 (LeftProduct k a)

    leftmul k (x1,x2,x3,x4) = (k *. x1, k *. x2, k *. x3, k *. x4)
    
instance LeftAction k a => LeftAction k (UniTuple5 a) where
    type LeftProduct k (UniTuple5 a) = UniTuple5 (LeftProduct k a)

    leftmul k (x1,x2,x3,x4,x5) = (k *. x1, k *. x2, k *. x3, k *. x4, k *. x5)
    