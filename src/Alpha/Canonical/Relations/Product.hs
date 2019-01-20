-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Product
(
    Product,
    NProduct(..), 
    NFactor(..), 
    Projector(..), 
    Productive(..),
    Product1(..), 
    Product2(..), 
    Product3(..), 
    Product4(..), 
    Product5(..),

) where
import Alpha.Canonical.Elementary

-- Defines an arity-1 heterogenous product
data Product1 a1 
    = Product1 !a1
        deriving (Eq, Ord, Data, Generic, Typeable)

-- Defines an arity-1 homogenous product
type UniProduct1 a = Product1 a

-- Defines an arity-2 heterogenous product
data Product2 a1 a2 
    = Product2 !a1 !a2
        deriving (Eq, Ord, Data,Generic, Typeable)

-- Defines an arity-2 homogenous product        
type UniProduct2 a = Product2 a a

-- Defines an arity-3 heterogenous product
data Product3 a1 a2 a3 
    = Product3 !a1 !a2 !a3
        deriving (Eq, Ord, Data,Generic, Typeable)

-- Defines an arity-3 homogenous product        
type UniProduct3 a = Product3 a a a

-- Defines an arity-4 heterogenous product
data Product4 a1 a2 a3 a4 
    = Product4 !a1 !a2 !a3 !a4
        deriving (Eq, Ord, Data,Generic, Typeable)

-- Defines an arity-4 homogenous product        
type UniProduct4 a = Product4 a a a a

-- Defines an arity-5 heterogenous product        
data Product5 a1 a2 a3 a4 a5 
    = Product5 !a1 !a2 !a3 !a4 !a5
        deriving (Eq, Ord, Data,Generic, Typeable)

-- Defines an arity-5 homogenous product        
type UniProduct5 a = Product5 a a a a a

-- | Defines the canonical bipartite product
data family Product a b

type family NProduct (n::Nat) a = r | r -> n a where
    NProduct 1 (Tuple1 a1) = Product1 a1 
    NProduct 2 (Tuple2 a1 a2) = Product2 a1 a2
    NProduct 3 (Tuple3 a1 a2 a3) = Product3 a1 a2 a3
    NProduct 4 (Tuple4 a1 a2 a3 a4) = Product4 a1 a2 a3 a4
    NProduct 5 (Tuple5 a1 a2 a3 a4 a5) = Product5 a1 a2 a3 a4 a5

type family NFactor (n::Nat) a = r  where
    NFactor 1 (Product1 a1) = a1
    NFactor 1 (Product2 a1 a2) = a1
    NFactor 2 (Product2 a1 a2) = a2
    NFactor 1 (Product3 a1 a2 a3)= a1
    NFactor 2 (Product3 a1 a2 a3) = a2
    NFactor 3 (Product3 a1 a2 a3) = a3
    NFactor 1 (Product4 a1 a2 a3 a4) = a1
    NFactor 2 (Product4 a1 a2 a3 a4) = a2
    NFactor 3 (Product4 a1 a2 a3 a4) = a3
    NFactor 4 (Product4 a1 a2 a3 a4) = a4
    NFactor 1 (Product5 a1 a2 a3 a4 a5) = a1
    NFactor 2 (Product5 a1 a2 a3 a4 a5) = a2
    NFactor 3 (Product5 a1 a2 a3 a4 a5) = a3
    NFactor 4 (Product5 a1 a2 a3 a4 a5) = a4
    NFactor 5 (Product5 a1 a2 a3 a4 a5) = a5

class Projector (i::Nat) a where
    project::a -> NFactor i a

class Productive (n::Nat) a where
     product::a -> NProduct n a
    
instance Productive 2 (Tuple2 a1 a2) where
    product (a1,a2) = Product2 a1 a2
instance Productive 3 (Tuple3 a1 a2 a3) where
    product (a1,a2,a3) = Product3 a1 a2 a3
instance Productive 4 (Tuple4 a1 a2 a3 a4) where
    product (a1,a2,a3,a4) = Product4 a1 a2 a3 a4
instance Productive 5 (Tuple5 a1 a2 a3 a4 a5) where
    product (a1,a2,a3,a4,a5) = Product5 a1 a2 a3 a4 a5
            
instance Projector 1 (Product2 a1 a2) where
    project (Product2 a1 _) = a1
instance Projector 2 (Product2 a1 a2) where 
    project (Product2 _ a2) = a2

instance Projector 1 (Product3 a1 a2 a3) where 
    project (Product3 a1 _ _) = a1
instance Projector 2 (Product3 a1 a2 a3) where 
    project (Product3 _ a2 _) = a2
instance Projector 3 (Product3 a1 a2 a3) where 
    project (Product3 _ _ a3) = a3
    
instance Projector 1 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 _ _ _) = a1
instance Projector 2 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a2
instance Projector 3 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a3    
instance Projector 4 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a4

instance Projector 1 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 _ _ _ _) = a1
instance Projector 2 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 _ a2 _ _ _) = a2
instance Projector 3 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 _ _ a3 _ _) = a3    
instance Projector 4 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 _ _ _ a4 _) = a4
instance Projector 5 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 _ _ _ _ a5) = a5
                
instance Formattable a => Formattable (Product1 a) where
    format (Product1 a) = tuplestring [format a]
instance Formattable2 a1 a2 => Formattable (Product2 a1 a2) where
    format (Product2 a1 a2) = tuplestring [format a1, format a2]
instance Formattable3 a1 a2 a3 => Formattable (Product3 a1 a2 a3) where
    format (Product3 a1 a2 a3) = tuplestring [format a1, format a2, format a3]
instance Formattable4 a1 a2 a3 a4 => Formattable (Product4 a1 a2 a3 a4) where
    format (Product4 a1 a2 a3 a4) = tuplestring [format a1, format a2, format a3, format a4]        
instance Formattable5 a1 a2 a3 a4 a5 => Formattable (Product5 a1 a2 a3 a4 a5) where
    format (Product5 a1 a2 a3 a4 a5) = tuplestring [format a1, format a2, format a3, format a4, format a5]
    
instance Formattable a => Show (Product1 a) where    
    show = string . format
instance Formattable2 a1 a2 => Show (Product2 a1 a2) where    
    show = string . format
instance Formattable3 a1 a2 a3 => Show (Product3 a1 a2 a3) where    
    show = string . format
instance Formattable4 a1 a2 a3 a4 => Show (Product4 a1 a2 a3 a4) where    
    show = string . format    
instance Formattable5 a1 a2 a3 a4 a5 => Show (Product5 a1 a2 a3 a4 a5) where    
    show = string . format        

instance Functor (Product2 a) where
    fmap f ~ (Product2 a x) = Product2 a (f x)
        
instance Foldable (Product2 a) where
    foldMap f (Product2 _ b) = f b
    foldr f c (Product2 _ b) = f b c
    
instance Traversable (Product2 a) where
    traverse f (Product2 a b) = Product2 a <$> f b
    
instance Bifunctor Product2 where
    bimap f g ~ (Product2 a1 a2) = Product2 (f a1) (g a2)
        
instance Biapply Product2 where
    Product2 f g <<.>> Product2 a b = Product2 (f a) (g b)

instance Biapplicative Product2 where
    bipure = Product2        
    Product2 f g <<*>> Product2 x y = Product2 (f x) (g y)    
    biliftA2 f g (Product2 x y) (Product2 a b) = Product2 (f x a) (g y b)

instance Comonad (Product2 e) where
    duplicate src = Product2 (project @1 src) src
    extract src = project @2 src
    
instance Semigroup2 a1 a2 => Semigroup (Product2 a1 a2) where 
    (<>) (Product2 x1 x2) (Product2 y1 y2)  
        = Product2 (x1 <> y1) (x2 <> y2)

instance Semigroup3 a1 a2 a3 => Semigroup (Product3 a1 a2 a3) where 
    (<>) (Product3 x1 x2 x3) (Product3 y1 y2 y3)  
        = Product3 (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Semigroup4 a1 a2 a3 a4 => Semigroup (Product4 a1 a2 a3 a4) where 
    (<>) (Product4 x1 x2 x3 x4) (Product4 y1 y2 y3 y4)  
        = Product4 (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

instance Semigroup5 a1 a2 a3 a4 a5 => Semigroup (Product5 a1 a2 a3 a4 a5) where 
    (<>) (Product5 x1 x2 x3 x4 x5) (Product5 y1 y2 y3 y4 y5)  
        = Product5 (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5 <> y5)
    
instance Monoid2 a1 a2 => Monoid (Product2 a1 a2) where 
    mempty = Product2 mempty mempty

instance Monoid3 a1 a2 a3 => Monoid (Product3 a1 a2 a3) where 
    mempty = Product3 mempty mempty mempty

instance Monoid4 a1 a2 a3 a4 => Monoid (Product4 a1 a2 a3 a4) where
    mempty = Product4 mempty mempty mempty mempty

instance Monoid5 a1 a2 a3 a4 a5 => Monoid (Product5 a1 a2 a3 a4 a5) where
    mempty = Product5 mempty mempty mempty mempty mempty
        