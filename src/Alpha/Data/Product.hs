{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Product(
    Product,Productive(..),Factored(..),
    Product1(..), Product2(..), Product3(..), Product4(..), Product5(..),
    Product6(..), Product7(..), Product8(..), Product9(..)
    ) where
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
    Product (a1,a2,a3,a4,a5,a6,a7)  = Product7 a1 a2 a3 a4 a5 a6 a7
    Product (a1,a2,a3,a4,a5,a6,a7,a8)  = Product8 a1 a2 a3 a4 a5 a6 a7 a8
    Product (a1,a2,a3,a4,a5,a6,a7,a8,a9)  = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9

-- Characterizes types from which products can be constructed
class Productive a where    
    -- | Forms a product from a tuple
    prod::a -> Product a

    -- | Forms a tuple from a product
    tuple::Product a -> a
    
    
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
    
instance Productive (a1,a2) where
    prod (a1,a2) = Product2 a1 a2
    tuple (Product2 a1 a2) = (a1,a2) 

instance Productive (a1,a2,a3) where
    prod (a1,a2,a3) = Product3 a1 a2 a3
    tuple (Product3 a1 a2 a3) = (a1,a2,a3) 

instance Productive (a1,a2,a3,a4)  where
    prod (a1,a2,a3,a4) = Product4 a1 a2 a3 a4
    tuple (Product4 a1 a2 a3 a4) = (a1,a2,a3,a4) 

instance Productive (a1,a2,a3,a4,a5) where
    prod (a1,a2,a3,a4,a5) = Product5 a1 a2 a3 a4 a5
    tuple (Product5 a1 a2 a3 a4 a5) = (a1,a2,a3,a4,a5)         

instance Productive (a1,a2,a3,a4,a5,a6) where
    prod (a1,a2,a3,a4,a5,a6) = Product6 a1 a2 a3 a4 a5 a6
    tuple (Product6 a1 a2 a3 a4 a5 a6) = (a1,a2,a3,a4,a5,a6)         

instance Productive (a1,a2,a3,a4,a5,a6,a7) where
    prod (a1,a2,a3,a4,a5,a6,a7) = Product7 a1 a2 a3 a4 a5 a6 a7
    tuple (Product7 a1 a2 a3 a4 a5 a6 a7) = (a1,a2,a3,a4,a5,a6,a7)

instance Productive (a1,a2,a3,a4,a5,a6,a7,a8) where
    prod (a1,a2,a3,a4,a5,a6,a7,a8) = Product8 a1 a2 a3 a4 a5 a6 a7 a8
    tuple (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = (a1,a2,a3,a4,a5,a6,a7,a8)

instance Productive (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    prod (a1,a2,a3,a4,a5,a6,a7,a8,a9) = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    tuple (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
        
instance Counted (Product2 a1 a2) where count _  = 2
instance Counted (Product3 a1 a2 a3) where count _  = 3
instance Counted (Product4 a1 a2 a3 a4) where count _  = 4
instance Counted (Product5 a1 a2 a3 a4 a5) where count _  = 5
instance Counted (Product6 a1 a2 a3 a4 a5 a6) where count _  = 6
instance Counted (Product7 a1 a2 a3 a4 a5 a6 a7) where count _  = 7
instance Counted (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where count _  = 8
instance Counted (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where count _  = 9    

-- Characterizes an componentized type a, with n-ordered parts, 
-- for which a j-indexed component can be extracted
class Factored (n::Nat) (j::Nat) a b  where    
    -- | Extracts the j-th component from a
    factor::a -> b

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

-- Factorization of a 6-product    
instance Factored 6 1 (Product6 a1 a2 a3 a4 a5 a6) a1 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a1

instance Factored 6 2 (Product6 a1 a2 a3 a4 a5 a6) a2 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a2

instance Factored 6 3 (Product6 a1 a2 a3 a4 a5 a6) a3 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a3

instance Factored 6 4 (Product6 a1 a2 a3 a4 a5 a6) a4 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a4

instance Factored 6 5 (Product6 a1 a2 a3 a4 a5 a6) a5 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a5

instance Factored 6 6 (Product6 a1 a2 a3 a4 a5 a6) a6 where
    factor (Product6 a1 a2 a3 a4 a5 a6) = a6
    
type Additive2 x1 x2 = (Additive x1, Additive x2)
type Additive3 x1 x2 x3 = (Additive x1, Additive x2, Additive x3)
type Additive4 x1 x2 x3 x4 = (Additive x1, Additive x2, Additive x3, Additive x4)
type Additive5 x1 x2 x3 x4 x5 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5)
type Additive6 x1 x2 x3 x4 x5 x6 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6)
type Additive7 x1 x2 x3 x4 x5 x6 x7 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7)
type Additive8 x1 x2 x3 x4 x5 x6 x7 x8 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7, Additive x8)
type Additive9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = (Additive x1, Additive x2, Additive x3, Additive x4, Additive x5, Additive x6, Additive x7, Additive x8, Additive x9)
        
instance Additive2 x1 x2 => Additive (Product2 x1 x2) where
    add (Product2 a1 a2) (Product2 b1 b2)
        = Product2 (a1 + b1) (a2 + b2)

instance Additive3 a1 a2 a3 => Additive (Product3 a1 a2 a3) where
    add (Product3 a1 a2 a3) (Product3 b1 b2 b3)  
        = Product3 (a1 + b1) (a2 + b2) (a3 + b3)
    
instance Additive4 a1 a2 a3 a4 => Additive (Product4 a1 a2 a3 a4) where
    add (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4)  
        = Product4 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)
        
instance Additive5 a1 a2 a3 a4 a5 => Additive (Product5 a1 a2 a3 a4 a5) where
    add (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5)  
        = Product5 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5)

instance Additive6 a1 a2 a3 a4 a5 a6 => Additive (Product6 a1 a2 a3 a4 a5 a6) where
    add (Product6 a1 a2 a3 a4 a5 a6) (Product6 b1 b2 b3 b4 b5 b6)  
        = Product6 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5) (a6 + b6)
                
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

instance Reversible (Product7 a1 a2 a3 a4 a5 a6 a7) (Product7 a7 a6 a5 a4 a3 a2 a1) where
    reverse (Product7 a1 a2 a3 a4 a5 a6 a7) = Product7 a7 a6 a5 a4 a3 a2 a1 

type Nullary2 x1 x2 = (Nullary x1, Nullary x2)
type Nullary3 x1 x2 x3 = (Nullary x1, Nullary x2, Nullary x3)
type Nullary4 x1 x2 x3 x4 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4)
type Nullary5 x1 x2 x3 x4 x5 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5)
type Nullary6 x1 x2 x3 x4 x5 x6 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6)
type Nullary7 x1 x2 x3 x4 x5 x6 x7 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7)
type Nullary8 x1 x2 x3 x4 x5 x6 x7 x8 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7, Nullary x8)
type Nullary9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = (Nullary x1, Nullary x2, Nullary x3, Nullary x4, Nullary x5, Nullary x6, Nullary x7, Nullary x8, Nullary x9)
        
instance Nullary2 a1 a2 => Nullary (Product2 a1 a2) where
    zero = prod (zero,zero)

instance Nullary3 a1 a2 a3 => Nullary (Product3 a1 a2 a3) where
    zero = prod (zero,zero,zero)
    
instance Nullary4 a1 a2 a3 a4 => Nullary (Product4 a1 a2 a3 a4) where
    zero = prod (zero,zero,zero,zero)

instance Nullary5 a1 a2 a3 a4 a5 => Nullary (Product5 a1 a2 a3 a4 a5) where
    zero = prod (zero,zero,zero,zero,zero)

instance Nullary6 a1 a2 a3 a4 a5 a6 => Nullary (Product6 a1 a2 a3 a4 a5 a6) where
    zero = prod (zero,zero,zero,zero,zero,zero)

instance Nullary7 a1 a2 a3 a4 a5 a6 a7 => Nullary (Product7 a1 a2 a3 a4 a5 a6 a7) where
    zero = prod (zero,zero,zero,zero,zero,zero,zero)

instance Nullary8 a1 a2 a3 a4 a5 a6 a7 a8 => Nullary (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    zero = prod (zero,zero,zero,zero,zero,zero,zero,zero)

instance Nullary9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Nullary (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    zero = prod (zero,zero,zero,zero,zero,zero,zero,zero,zero)

type Invertible2 x1 x2 = (Invertible x1 x1, Invertible x2 x2)
type Invertible3 x1 x2 x3 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3)
type Invertible4 x1 x2 x3 x4 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4)
type Invertible5 x1 x2 x3 x4 x5 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5)
type Invertible6 x1 x2 x3 x4 x5 x6 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6)
type Invertible7 x1 x2 x3 x4 x5 x6 x7 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6, Invertible x7 x7)
type Invertible8 x1 x2 x3 x4 x5 x6 x7 x8 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6, Invertible x7 x7, Invertible x8 x8)
type Invertible9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = (Invertible x1 x1, Invertible x2 x2, Invertible x3 x3, Invertible x4 x4, Invertible x5 x5, Invertible x6 x6, Invertible x7 x7, Invertible x8 x8, Invertible x9 x9)
        
instance Invertible2 a1 a2 => Invertible (Product2 a1 a2) (Product2 a1 a2) where
    invert (Product2 a1 a2) = Product2 (invert a1) (invert a2)    
