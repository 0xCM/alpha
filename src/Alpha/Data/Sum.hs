-----------------------------------------------------------------------------
-- | Defines n-ary Sums
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Alpha.Data.Sum
(
    Sum1(..), Sum2(..), Sum3(..), Sum4(..), Sum5(..),
    Sum6(..), Sum7(..), Sum8(..), Sum9(..),
    NSum(..), Union(..),
    type (!+!),
    (!+), (+!)
)
where

import Alpha.Base
import Alpha.Canonical

-- A single-case sum
data Sum1 a1 
    = Sum11 !a1
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 2
data Sum2 a1 a2 
    = Sum21 !a1 | Sum22 !a2
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 3
data Sum3 a1 a2 a3 
    = Sum31 !a1 | Sum32 !a2 | Sum33 !a3
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 4
data Sum4 a1 a2 a3 a4
    = Sum41 !a1 | Sum42 !a2 | Sum43 !a3 | Sum44 !a4
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 5    
data Sum5 a1 a2 a3 a4 a5
    = Sum51 !a1 | Sum52 !a2 | Sum53 !a3 | Sum54 !a4 | Sum55 !a5
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 6    
data Sum6 a1 a2 a3 a4 a5 a6
    = Sum61 !a1 | Sum62 !a2 | Sum63 !a3 | Sum64 !a4 | Sum65 !a5
    | Sum66 !a6
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

-- A disjoint sum of arity 7    
data Sum7 a1 a2 a3 a4 a5 a6 a7
    = Sum71 !a1 | Sum72 !a2 | Sum73 !a3 | Sum74 !a4 | Sum75 !a5 
    | Sum76 !a6 | Sum77 !a7
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

-- A disjoint sum of arity 8    
data Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    = Sum81 !a1 | Sum82 !a2 | Sum83 !a3 | Sum84 !a4 | Sum85 !a5
    | Sum86 !a6 | Sum87 !a7 | Sum88 !a8
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

-- A disjoint sum of arity 9    
data Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    = Sum91 !a1 | Sum92 !a2 | Sum93 !a3 | Sum94 !a4 | Sum95 !a5
    | Sum96 !a6 | Sum97 !a7 | Sum98 !a8 | Sum99 !a9
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

type family Union a | a -> a where
    Union (Sum1 a1) = Sum1 a1    
    Union (Sum2 a1 a2) = Sum2 a1 a2
    Union (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    Union (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    Union (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    Union (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6
    Union (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7
    Union (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) = Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    Union (Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9

type a !+! b = Sum2 a b

-- Constructs a left-biased sum
(!+)::a -> a !+! b
(!+) = Sum21

-- Constructs a right-biased sum
(+!)::b -> a !+! b
(+!) = Sum22


instance IndexedChoice (Sum2 a1 a2) where
    choiceix (Sum21 _) = 1
    choiceix (Sum22 _) = 2
    
instance IndexedChoice (Sum3 a1 a2 a3) where
    choiceix (Sum31 _) = 1
    choiceix (Sum32 _) = 2
    choiceix (Sum33 _) = 3

instance IndexedChoice (Sum4 a1 a2 a3 a4) where
    choiceix (Sum41 _) = 1
    choiceix (Sum42 _) = 2
    choiceix (Sum43 _) = 3
    choiceix (Sum44 _) = 4

instance IndexedChoice (Sum5 a1 a2 a3 a4 a5) where
    choiceix (Sum51 _) = 1
    choiceix (Sum52 _) = 2
    choiceix (Sum53 _) = 3
    choiceix (Sum54 _) = 4
    choiceix (Sum55 _) = 5

-- A disjoint sum of arity n    
type family NSum (n::Nat) a | n -> a where
    NSum 1 (Sum1 a1) = Sum1 a1
    NSum 2 (Sum2 a1 a2) = Sum2 a1 a2
    NSum 3 (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    NSum 4 (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    NSum 5 (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    NSum 6 (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6
    NSum 7 (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7
    NSum 8 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) = Sum8 a1 a2 a3 a4 a5 a6 a7 a8
    NSum 9 (Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Sum9 a1 a2 a3 a4 a5 a6 a7 a8 a9

instance  Semigroup (Sum2 a1 a2) where
    Sum21 _ <> a2 = a2
    a1      <> _ = a1

instance Functor (Sum2 a1) where
    fmap _ (Sum21 a1) = Sum21 a1
    fmap f (Sum22 a2) = Sum22 (f a2)

-- instance Alt (Sum2 a1) where
--     f <!> (Sum21 a1) = f <$> a1
--     f <!> (Sum22 a2) = f <$> a2
    
instance Applicative (Sum2 a1) where
    pure            = Sum22
    Sum21 a1 <*> _  = Sum21 a1
    Sum22 f  <*> a2 = fmap f a2

instance Foldable (Sum2 a1) where
    foldMap _ (Sum21 _) = mempty
    foldMap f (Sum22 y) = f y

    foldr _ z (Sum21 _) = z
    foldr f z (Sum22 y) = f y z

instance Traversable (Sum2 a1) where
    traverse _ (Sum21 a1) = pure (Sum21 a1)
    traverse f (Sum22 a2) = Sum22 <$> f a2

instance Monad (Sum2 a1) where
    Sum21 a1 >>= _ = Sum21 a1
    Sum22 a2 >>= k = k a2
        
instance Apply (Sum2 a1) where
    Sum21 a1  <.> _         = Sum21 a1
    Sum22 _   <.> Sum21 a1  = Sum21 a1
    Sum22 f   <.> Sum22 a2  = Sum22 (f a2)
    
    Sum21 a1  <.  _       = Sum21 a1
    Sum22 _   <. Sum21 a1 = Sum21 a1
    Sum22 a1  <. Sum22 _  = Sum22 a1
    
    Sum21 a1   .> _       = Sum21 a1
    Sum22 _  .> Sum21 a1  = Sum21 a1
    Sum22 _  .> Sum22 a2  = Sum22 a2
      

-- instance Alt (Sum3 a1 a2) where
--     f <!> (Sum31 a1) = Sum31 (f a1)
--     f <!> (Sum32 a2) = Sum32 (f a2)    
--     f <!> (Sum33 a3) = Sum32 (f a3)        

instance Functor (Sum3 a1 a2) where
    fmap _ (Sum31 a1) = Sum31 a1
    fmap _ (Sum32 a2) = Sum32 a2
    fmap f (Sum33 a3) = Sum33 (f a3)

instance Functor (Sum4 a1 a2 a3) where
    fmap _ (Sum41 a1) = Sum41 a1
    fmap _ (Sum42 a2) = Sum42 a2
    fmap _ (Sum43 a3) = Sum43 a3
    fmap f (Sum44 a4) = Sum44 (f a4)

instance Bifunctor Sum2 where
    bimap f1 _ (Sum21 a1) = Sum21 (f1 a1)
    bimap _ f2 (Sum22 a2) = Sum22 (f2 a2)

instance Bifoldable Sum2 where
    bifoldMap f1 _ (Sum21 a1) = f1 a1
    bifoldMap _ f2 (Sum22 a2) = f2 a2
      
instance Bitraversable Sum2 where
    bitraverse f1 _ (Sum21 a1) = Sum21 <$> f1 a1
    bitraverse _ f2 (Sum22 a2) = Sum22 <$> f2 a2    