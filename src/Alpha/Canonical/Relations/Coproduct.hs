{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Canonical.Relations.Coproduct
(
    DisjointUnion(..),
    disjoint,
    Sum1(..), Sum2(..), Sum3(..), Sum4(..), Sum5(..),
    type (!+!),
    (!+), (+!),

    Coproductive(..),
    comap1, comap2, comap3, comap4, comap5,

) where
import Alpha.Base
import Alpha.Canonical.Common
import Alpha.Canonical.Relations.Tripling
import Alpha.Canonical.Relations.Product
import Alpha.Canonical.Relations.Functions

-- | Represents a dijoint union of elements
-- See https://en.wikipedia.org/wiki/Disjoint_union
newtype DisjointUnion a b = DisjointUnion (a, b)

type instance Tripled a b (DisjointUnion a b) = (DisjointUnion a b)

-- | Constructs a disjoint union
disjoint::a -> b -> DisjointUnion a b
disjoint a b = DisjointUnion (a,b)
    
-- A single-case sum
data Sum1 a1 
    = Sum1 !a1
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

type family Sum a | a -> a where
    Sum (Sum1 a1) = Sum1 a1    
    Sum (Sum2 a1 a2) = Sum2 a1 a2
    Sum (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    Sum (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    Sum (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    
type a !+! b = Sum2 a b

-- Constructs a left-biased sum
(!+)::a -> a !+! b
(!+) = Sum21

-- Constructs a right-biased sum
(+!)::b -> a !+! b
(+!) = Sum22
    
class Coproductive (n::Nat) a b where
    -- | Injects an a-value into a b-valued coproduct
    inject::b -> Sum a

instance Coproductive 1 (Sum1 a) a where
    inject a = Sum1 a    
    
instance Coproductive 1 (Sum2 a1 a2) a1 where
    inject a = Sum21 a
instance Coproductive 2 (Sum2 a1 a2) a2 where
    inject a = Sum22 a
        
instance Coproductive 1 (Sum3 a1 a2 a3) a1 where
    inject a = Sum31 a
instance Coproductive 2 (Sum3 a1 a2 a3) a2 where
    inject a = Sum32 a
instance Coproductive 3 (Sum3 a1 a2 a3) a3 where
    inject a = Sum33 a
        
instance Coproductive 1 (Sum4 a1 a2 a3 a4) a1 where
    inject a = Sum41 a
instance Coproductive 2 (Sum4 a1 a2 a3 a4) a2 where
    inject a = Sum42 a
instance Coproductive 3 (Sum4 a1 a2 a3 a4) a3 where
    inject a = Sum43 a
instance Coproductive 4 (Sum4 a1 a2 a3 a4) a4 where
    inject a = Sum44 a
                
instance Coproductive 5 (Sum5 a1 a2 a3 a4 a5) a1 where
    inject a = Sum51 a
instance Coproductive 5 (Sum5 a1 a2 a3 a4 a5) a2 where
    inject a = Sum52 a
instance Coproductive 5 (Sum5 a1 a2 a3 a4 a5) a3 where
    inject a = Sum53 a
instance Coproductive 5 (Sum5 a1 a2 a3 a4 a5) a4 where
    inject a = Sum54 a
instance Coproductive 5 (Sum5 a1 a2 a3 a4 a5) a5 where
    inject a = Sum55 a

type CoFunc1 f a b = f ~ F1 a b
comap1::CoFunc1 f a b => Product1 f -> (Sum1 a) -> (Sum1 b)
comap1 (Product1 f ) (Sum1 a) = Sum1 (f a)

type CoFunc2 f1 a1 b1 f2 a2 b2 = (f1 ~ F1 a1 b1, f2 ~ F1 a2 b2)
comap2::CoFunc2 f1 a1 b1 f2 a2 b2 => Product2 f1 f2 -> (Sum2 a1 a2) -> (Sum2 b1 b2)
comap2 (Product2 f1 _ ) (Sum21 a1) = Sum21 (f1 a1)
comap2 (Product2 _ f2 ) (Sum22 a2) = Sum22 (f2 a2)  

type CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3 = (CoFunc2 f1 a1 b1 f2 a2 b2, f3 ~ F1 a3 b3)
comap3::CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3 => Product3 f1 f2 f3 -> (Sum3 a1 a2 a3) -> (Sum3 b1 b2 b3)
comap3 (Product3 f1 _ _) (Sum31 a1) = Sum31 (f1 a1)
comap3 (Product3 _ f2 _) (Sum32 a2) = Sum32 (f2 a2)  
comap3 (Product3 _ _ f3) (Sum33 a3) = Sum33 (f3 a3)

type CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 = (CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3, f4 ~ F1 a4 b4)
comap4::(CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4)  => Product4 f1 f2 f3 f4 -> (Sum4 a1 a2 a3 a4) -> (Sum4 b1 b2 b3 b4)
comap4 (Product4 f1 _ _ _) (Sum41 a1) = Sum41 (f1 a1)
comap4 (Product4 _ f2 _ _) (Sum42 a2) = Sum42 (f2 a2)  
comap4 (Product4 _ _ f3 _) (Sum43 a3) = Sum43 (f3 a3)
comap4 (Product4 _ _ _ f4) (Sum44 a4) = Sum44 (f4 a4)    

type CoFunc5 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5  = (CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4, f5 ~ F1 a5 b5)
comap5::CoFunc5 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5 => Product5 f1 f2 f3 f4 f5 -> Sum5 a1 a2 a3 a4 a5 -> Sum5 b1 b2 b3 b4 b5
comap5 (Product5 f1 _ _ _ _) (Sum51 a1) = Sum51 (f1 a1)
comap5 (Product5 _ f2 _ _ _) (Sum52 a2) = Sum52 (f2 a2)  
comap5 (Product5 _ _ f3 _ _) (Sum53 a3) = Sum53 (f3 a3)
comap5 (Product5 _ _ _ f4 _) (Sum54 a4) = Sum54 (f4 a4)    
comap5 (Product5 _ _ _ _ f5) (Sum55 a5) = Sum55 (f5 a5)    
    
instance  Semigroup (Sum2 a1 a2) where
    Sum21 _ <> a2 = a2
    a1      <> _ = a1

instance Functor (Sum2 a1) where
    fmap _ (Sum21 a1) = Sum21 a1
    fmap f (Sum22 a2) = Sum22 (f a2)
    
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

instance Bifunctor Sum2 where
    bimap f1 _ (Sum21 a1) = Sum21 (f1 a1)
    bimap _ f2 (Sum22 a2) = Sum22 (f2 a2)

instance Bifoldable Sum2 where
    bifoldMap f1 _ (Sum21 a1) = f1 a1
    bifoldMap _ f2 (Sum22 a2) = f2 a2
        
instance Bitraversable Sum2 where
    bitraverse f1 _ (Sum21 a1) = Sum21 <$> f1 a1
    bitraverse _ f2 (Sum22 a2) = Sum22 <$> f2 a2        