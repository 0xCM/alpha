{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Product(Product,Productive(..),Factored(..)) where
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators

import qualified Data.Vector as V

-- Defines a family of product types
type family Product a | a -> a where    
    Product (a1,a2) = Product2 a1 a2
    Product (a1,a2,a3) = Product3 a1 a2 a3
    Product (a1,a2,a3,a4) = Product4 a1 a2 a3 a4
    Product (a1,a2,a3,a4,a5)  = Product5 a1 a2 a3 a4 a5
    Product (a1,a2,a3,a4,a5,a6)  = Product6 a1 a2 a3 a4 a5 a6 

-- Characterizes types from which products can be constructed
class Productive a where    
    -- | Forms an a-valued product
    product::a -> Product a
    -- | Reverts an a-valued product
    unproduct::Product a -> a
    
-- Characterizes an componentized type a, with n-ordered parts, 
-- for which a j-indexed component can be extracted
class Factored (n::Nat) (j::Nat) a b  where    
    -- | Extracts the j-th component from a
    factor::a -> b
    
data Product1 a1 = Product1 
    {-# UNPACK #-} !a1
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

data Product2 a1 a2 = Product2 
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product3 a1 a2 a3 = Product3 
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product4 a1 a2 a3 a4 = Product4 
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product5 a1 a2 a3 a4 a5 = Product5
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    {-# UNPACK #-} !a5
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product6 a1 a2 a3 a4 a5 a6 = Product6
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    {-# UNPACK #-} !a5
    {-# UNPACK #-} !a6
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product7 a1 a2 a3 a4 a5 a6 a7 = Product7
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    {-# UNPACK #-} !a5
    {-# UNPACK #-} !a6
    {-# UNPACK #-} !a7
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product8 a1 a2 a3 a4 a5 a6 a7 a8 = Product8
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    {-# UNPACK #-} !a5
    {-# UNPACK #-} !a6
    {-# UNPACK #-} !a7
    {-# UNPACK #-} !a8
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Product9
    {-# UNPACK #-} !a1 
    {-# UNPACK #-} !a2 
    {-# UNPACK #-} !a3
    {-# UNPACK #-} !a4
    {-# UNPACK #-} !a5
    {-# UNPACK #-} !a6
    {-# UNPACK #-} !a7
    {-# UNPACK #-} !a8
    {-# UNPACK #-} !a9
    deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

-- instance Productive 1 a1 (Product1 a1) where
--     product = Product1
--     unproduct (Product1 a1) = a1
    
instance Productive (a1,a2) where
    product (a1,a2) = Product2 a1 a2
    unproduct (Product2 a1 a2) = (a1,a2) 

instance Productive (a1,a2,a3) where
    product (a1,a2,a3) = Product3 a1 a2 a3
    unproduct (Product3 a1 a2 a3) = (a1,a2,a3) 

instance Productive (a1,a2,a3,a4)  where
    product (a1,a2,a3,a4) = Product4 a1 a2 a3 a4
    unproduct (Product4 a1 a2 a3 a4) = (a1,a2,a3,a4) 

instance Productive (a1,a2,a3,a4,a5) where
    product (a1,a2,a3,a4,a5) = Product5 a1 a2 a3 a4 a5
    unproduct (Product5 a1 a2 a3 a4 a5) = (a1,a2,a3,a4,a5)         

        
    
type Additive2 x1 x2 = (Additive x1, Additive x2)
type Additive3 x1 x2 x3 = (Additive x1, Additive x2, Additive x3)
type Additive4 x1 x2 x3 x4 = (Additive x1, Additive x2, Additive x3, Additive x4)
type Additive5 x1 x2 x3 x4 x5 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5)
type Additive6 x1 x2 x3 x4 x5 x6 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6)
type Additive7 x1 x2 x3 x4 x5 x6 x7 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7)
type Additive8 x1 x2 x3 x4 x5 x6 x7 x8 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7, Additive x8)
type Additive9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7, Additive x8, Additive x9)

type Nullary2 x1 x2 = (Nullary x1, Nullary x2)
type Nullary3 x1 x2 x3 = (Nullary x1, Nullary x2, Nullary x3)
type Nullary4 x1 x2 x3 x4 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4)
type Nullary5 x1 x2 x3 x4 x5 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5)
type Nullary6 x1 x2 x3 x4 x5 x6 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6)
type Nullary7 x1 x2 x3 x4 x5 x6 x7 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7)
type Nullary8 x1 x2 x3 x4 x5 x6 x7 x8 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7, Nullary x8)
type Nullary9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7, Nullary x8, Nullary x9)

type Invertible2 x1 x2 = (Invertible x1 x1, Invertible x2 x2)
type Invertible3 x1 x2 x3 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3)
type Invertible4 x1 x2 x3 x4 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4)
type Invertible5 x1 x2 x3 x4 x5 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5)
type Invertible6 x1 x2 x3 x4 x5 x6 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6)
type Invertible7 x1 x2 x3 x4 x5 x6 x7 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6, Invertible x7 x7)

instance Factored 2 1 (Product2 x1 x2) x1 where
    factor (Product2 x1 x2) = x1
    
instance Factored 2 2 (Product2 x1 x2) x2 where
    factor (Product2 x1 x2) = x2
    
instance Factored 3 1 (Product3 x1 x2 x3) x1 where
    factor (Product3 x1 x2 x3) = x1

instance Factored 3 2 (Product3 x1 x2 x3) x2 where
    factor (Product3 x1 x2 x3) = x2

instance Factored 3 3 (Product3 x1 x2 x3) x3 where
    factor (Product3 x1 x2 x3) = x3

instance Factored 4 1 (Product4 x1 x2 x3 x4) x1 where
    factor (Product4 x1 x2 x3 x4) = x1

instance Factored 4 2 (Product4 x1 x2 x3 x4) x2 where
    factor (Product4 x1 x2 x3 x4) = x2

instance Factored 4 3 (Product4 x1 x2 x3 x4) x3 where
    factor (Product4 x1 x2 x3 x4) = x3

instance Factored 4 4 (Product4 x1 x2 x3 x4) x4 where
    factor (Product4 x1 x2 x3 x4) = x4

instance Factored 5 1 (Product5 x1 x2 x3 x4 x5) x1 where
    factor (Product5 x1 x2 x3 x4 x5) = x1

instance Factored 5 2 (Product5 x1 x2 x3 x4 x5) x2 where
    factor (Product5 x1 x2 x3 x4 x5) = x2

instance Factored 5 3 (Product5 x1 x2 x3 x4 x5) x3 where
    factor (Product5 x1 x2 x3 x4 x5) = x3

instance Factored 5 4 (Product5 x1 x2 x3 x4 x5) x4 where
    factor (Product5 x1 x2 x3 x4 x5) = x4

instance Factored 5 5 (Product5 x1 x2 x3 x4 x5) x5 where
    factor (Product5 x1 x2 x3 x4 x5) = x5

    
instance Additive2 x1 x2 => Additive (Product2 x1 x2) where
    add (Product2 a1 a2) (Product2 b1 b2)
        = Product2 (a1 + b1) (a2 + b2)

instance Additive3 x1 x2 x3 => Additive (Product3 x1 x2 x3) where
    add (Product3 a1 a2 a3) (Product3 b1 b2 b3)  
        = Product3 (a1 + b1) (a2 + b2) (a3 + b3)
    
instance Additive4 x1 x2 x3 x4 => Additive (Product4 x1 x2 x3 x4) where
    add (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4)  
        = Product4 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)
        
instance Additive5 x1 x2 x3 x4 x5 => Additive (Product5 x1 x2 x3 x4 x5) where
    add (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5)  
        = Product5 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5)
            
instance Reversible (Product2 a1 a2) (Product2 a2 a1) where
    reverse (Product2 a1 a2) = Product2 a2 a1

instance Reversible (Product3 a1 a2 a3) (Product3 a3 a2 a1) where
    reverse (Product3 a1 a2 a3) = Product3 a3 a2 a1
    
instance Reversible (Product4 a1 a2 a3 a4) (Product4 a4 a3 a2 a1) where
    reverse (Product4 a1 a2 a3 a4) = Product4 a4 a3 a2 a1
    
instance Reversible (Product5 a1 a2 a3 a4 a5) (Product5 a5 a4 a3 a2 a1) where
    reverse (Product5 a1 a2 a3 a4 a5) = Product5 a5 a4 a3 a2 a1

instance Reversible (Product6 a1 a2 a3 a4 a5 a6) (Product6 a6 a5 a4 a3 a2 a1) where
    reverse (Product6 a1 a2 a3 a4 a5 a6) = Product6 a6 a5 a4 a3 a2 a1
    

instance Nullary2 x1 x2 => Nullary (Product2 x1 x2) where
    zero = product (zero,zero)

instance Nullary3 x1 x2 x3 => Nullary (Product3 x1 x2 x3) where
    zero = product (zero,zero,zero)
    
instance Nullary4 x1 x2 x3 x4 => Nullary (Product4 x1 x2 x3 x4) where
    zero = product (zero,zero,zero,zero)

instance Nullary5 x1 x2 x3 x4 x5 => Nullary (Product5 x1 x2 x3 x4 x5) where
    zero = product (zero,zero,zero,zero,zero)

instance Nullary6 x1 x2 x3 x4 x5 x6 => Nullary (x1,x2,x3,x4,x5,x6) where
    zero = (zero,zero,zero,zero,zero,zero)


instance Invertible2 x1 x2 => Invertible (x1,x2) (x1,x2) where
    invert (a1,a2) = (invert a1, invert a2)    