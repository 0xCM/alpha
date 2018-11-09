{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Product
(
    Product,Productive(..),Factored(..),
    Product1(..), Product2(..), Product3(..), Product4(..), Product5(..),
    Product6(..), Product7(..), Product8(..), Product9(..),
    type (!*!), (!*!)
) where
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators
import qualified Control.Category as C
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

type family Factor (j::Nat) a where
    Factor 1 (Product2 a1 a2) = a1
    Factor 2 (Product2 a1 a2) = a2
    
    Factor 1 (Product3 a1 a2 a3) = a1
    Factor 2 (Product3 a1 a2 a3) = a2
    Factor 3 (Product3 a1 a2 a3) = a3
    
    Factor 1 (Product4 a1 a2 a3 a4) = a1
    Factor 2 (Product4 a1 a2 a3 a4) = a2
    Factor 3 (Product4 a1 a2 a3 a4) = a3
    Factor 4 (Product4 a1 a2 a3 a4) = a4

    Factor 1 (Product5 a1 a2 a3 a4 a5) = a1
    Factor 2 (Product5 a1 a2 a3 a4 a5) = a2
    Factor 3 (Product5 a1 a2 a3 a4 a5) = a3
    Factor 4 (Product5 a1 a2 a3 a4 a5) = a4
    Factor 5 (Product5 a1 a2 a3 a4 a5) = a5

    Factor 1 (Product6 a1 a2 a3 a4 a5 a6) = a1
    Factor 2 (Product6 a1 a2 a3 a4 a5 a6) = a2
    Factor 3 (Product6 a1 a2 a3 a4 a5 a6) = a3
    Factor 4 (Product6 a1 a2 a3 a4 a5 a6) = a4
    Factor 5 (Product6 a1 a2 a3 a4 a5 a6) = a5
    Factor 6 (Product6 a1 a2 a3 a4 a5 a6) = a6

    Factor 1 (Product7 a1 a2 a3 a4 a5 a6 a7) = a1
    Factor 2 (Product7 a1 a2 a3 a4 a5 a6 a7) = a2
    Factor 3 (Product7 a1 a2 a3 a4 a5 a6 a7) = a3
    Factor 4 (Product7 a1 a2 a3 a4 a5 a6 a7) = a4
    Factor 5 (Product7 a1 a2 a3 a4 a5 a6 a7) = a5
    Factor 6 (Product7 a1 a2 a3 a4 a5 a6 a7) = a6
    Factor 7 (Product7 a1 a2 a3 a4 a5 a6 a7) = a7

    Factor 1 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a1
    Factor 2 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a2
    Factor 3 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a3
    Factor 4 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a4
    Factor 5 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a5
    Factor 6 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a6
    Factor 7 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a7
    Factor 8 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a8

    Factor 1 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a1
    Factor 2 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a2
    Factor 3 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a3
    Factor 4 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a4
    Factor 5 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a5
    Factor 6 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a6
    Factor 7 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a7
    Factor 8 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a8
    Factor 9 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a9    

type a !*! b = Product2 a b

-- Constructs a product
(!*!)::a -> b -> a !*! b
(!*!) = Product2
infixl 5 !*!

-- Characterizes types from which products can be constructed
class Productive a where    
    -- | Forms a product from a tuple
    prod::a -> Product a

    -- | Forms a tuple from a product
    tuple::Product a -> a

-- Characterizes a componentized type a with n components indexed by j
class Factored (j::Nat) a where    
    -- | Extracts the j-th component from a
    factor::a -> Factor j a
    
data Product1 a1 
    = Product1 !a1
        deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)

data Product2 a1 a2 
    = Product2 !a1 !a2
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product3 a1 a2 a3 
    = Product3 !a1 !a2 !a3
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product4 a1 a2 a3 a4 
    = Product4 !a1 !a2 !a3 !a4
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product5 a1 a2 a3 a4 a5 
    = Product5 !a1 !a2 !a3 !a4 !a5
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product6 a1 a2 a3 a4 a5 a6 
    = Product6 !a1 !a2 !a3 !a4 !a5 !a6
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product7 a1 a2 a3 a4 a5 a6 a7 
    = Product7 !a1 !a2 !a3 !a4 !a5 !a6 !a7
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product8 a1 a2 a3 a4 a5 a6 a7 a8 
    = Product8 !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8
        deriving (Eq, Ord, Data,Generic, Typeable, Functor, Show)

data Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9 
    = Product9 !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8 !a9
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
        
instance Factored 1 (Product2 a1 a2) where 
    factor (Product2 x1 x2) = x1    
instance Factored 2 (Product2 a1 a2) where 
    factor (Product2 x1 x2) = x2
    
instance Factored 1 (Product3 a1 a2 a3) where 
    factor (Product3 a1 a2 a3) = a1
instance Factored 2 (Product3 a1 a2 a3) where 
    factor (Product3 a1 a2 a3) = a2
instance Factored 3 (Product3 a1 a2 a3) where 
    factor (Product3 a1 a2 a3) = a3

instance Factored 1 (Product4 a1 a2 a3 a4) where 
    factor (Product4 a1 a2 a3 a4) = a1
instance Factored 2 (Product4 a1 a2 a3 a4) where 
    factor (Product4 a1 a2 a3 a4) = a2
instance Factored 3 (Product4 a1 a2 a3 a4) where 
    factor (Product4 a1 a2 a3 a4) = a3    
instance Factored 4 (Product4 a1 a2 a3 a4) where 
    factor (Product4 a1 a2 a3 a4) = a4

instance Factored 1 (Product5 a1 a2 a3 a4 a5) where 
    factor (Product5 a1 a2 a3 a4 a5) = a1
instance Factored 2 (Product5 a1 a2 a3 a4 a5) where 
    factor (Product5 a1 a2 a3 a4 a5) = a2
instance Factored 3 (Product5 a1 a2 a3 a4 a5) where 
    factor (Product5 a1 a2 a3 a4 a5) = a3    
instance Factored 4 (Product5 a1 a2 a3 a4 a5) where 
    factor (Product5 a1 a2 a3 a4 a5) = a4
instance Factored 5 (Product5 a1 a2 a3 a4 a5) where 
    factor (Product5 a1 a2 a3 a4 a5) = a5

instance Factored 1 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a1
instance Factored 2 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a2
instance Factored 3 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a3    
instance Factored 4 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a4
instance Factored 5 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a5
instance Factored 6 (Product6 a1 a2 a3 a4 a5 a6) where 
    factor (Product6 a1 a2 a3 a4 a5 a6) = a6

instance Factored 1 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a1
instance Factored 2 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a2
instance Factored 3 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a3    
instance Factored 4 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a4
instance Factored 5 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a5
instance Factored 6 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a6
instance Factored 7 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    factor (Product7 a1 a2 a3 a4 a5 a6 a7) = a7

instance Factored 1 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a1
instance Factored 2 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a2
instance Factored 3 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a3    
instance Factored 4 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a4
instance Factored 5 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a5
instance Factored 6 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a6
instance Factored 7 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a7
instance Factored 8 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    factor (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a8

instance Factored 1 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a1
instance Factored 2 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a2
instance Factored 3 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a3    
instance Factored 4 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a4
instance Factored 5 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a5
instance Factored 6 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a6
instance Factored 7 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a7
instance Factored 8 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a8
instance Factored 9 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    factor (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a9
                
instance Counted (Product2 a1 a2) where count _  = 2
instance Counted (Product3 a1 a2 a3) where count _  = 3
instance Counted (Product4 a1 a2 a3 a4) where count _  = 4
instance Counted (Product5 a1 a2 a3 a4 a5) where count _  = 5
instance Counted (Product6 a1 a2 a3 a4 a5 a6) where count _  = 6
instance Counted (Product7 a1 a2 a3 a4 a5 a6 a7) where count _  = 7
instance Counted (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where count _  = 8
instance Counted (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where count _  = 9    
    
type Additive2 a1 a2 = (Additive a1, Additive a2)
type Additive3 a1 a2 a3 = (Additive2 a1 a2, Additive a3)
type Additive4 a1 a2 a3 a4 = (Additive3 a1 a2 a3, Additive a4)
type Additive5 a1 a2 a3 a4 a5 = (Additive4 a1 a2 a3 a4, Additive a5)
type Additive6 a1 a2 a3 a4 a5 a6 = (Additive5 a1 a2 a3 a4 a5, Additive a6)
type Additive7 a1 a2 a3 a4 a5 a6 a7 = (Additive6 a1 a2 a3 a4 a5 a6, Additive a7)
type Additive8 a1 a2 a3 a4 a5 a6 a7 a8 = (Additive7 a1 a2 a3 a4 a5 a6 a7, Additive a8)
type Additive9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Additive8 a1 a2 a3 a4 a5 a6 a7 a8, Additive a9)

instance Additive2 a1 a2 => Additive (Product2 a1 a2) where
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
instance Additive7 a1 a2 a3 a4 a5 a6 a7 => Additive (Product7 a1 a2 a3 a4 a5 a6 a7) where
    add (Product7 a1 a2 a3 a4 a5 a6 a7) (Product7 b1 b2 b3 b4 b5 b6 b7)  
        = Product7 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7)
instance Additive8 a1 a2 a3 a4 a5 a6 a7 a8 => Additive (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    add (Product8 a1 a2 a3 a4 a5 a6 a7 a8) (Product8 b1 b2 b3 b4 b5 b6 b7 b8)  
        = Product8 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7) (a8 + b8)
instance Additive9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Additive (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    add (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) (Product9 b1 b2 b3 b4 b5 b6 b7 b8 b9)  
        = Product9 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7) (a8 + b8) (a9 + b9)

type Subtractive2 a1 a2 = (Subtractive a1, Subtractive a2)
type Subtractive3 a1 a2 a3 = (Subtractive2 a1 a2, Subtractive a3)
type Subtractive4 a1 a2 a3 a4 = (Subtractive3 a1 a2 a3, Subtractive a4)
type Subtractive5 a1 a2 a3 a4 a5 = (Subtractive4 a1 a2 a3 a4, Subtractive a5)
type Subtractive6 a1 a2 a3 a4 a5 a6 = (Subtractive5 a1 a2 a3 a4 a5, Subtractive a6)
type Subtractive7 a1 a2 a3 a4 a5 a6 a7 = (Subtractive6 a1 a2 a3 a4 a5 a6, Subtractive a7)
type Subtractive8 a1 a2 a3 a4 a5 a6 a7 a8 = (Subtractive7 a1 a2 a3 a4 a5 a6 a7, Subtractive a8)
type Subtractive9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Subtractive8 a1 a2 a3 a4 a5 a6 a7 a8, Subtractive a9)

instance Subtractive2 a1 a2 => Subtractive (Product2 a1 a2) where
    sub (Product2 a1 a2) (Product2 b1 b2)
        = Product2 (a1 - b1) (a2 - b2)
instance Subtractive3 a1 a2 a3 => Subtractive (Product3 a1 a2 a3) where
    sub (Product3 a1 a2 a3) (Product3 b1 b2 b3)  
        = Product3 (a1 - b1) (a2 - b2) (a3 - b3)    
instance Subtractive4 a1 a2 a3 a4 => Subtractive (Product4 a1 a2 a3 a4) where
    sub (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4)  
        = Product4 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4)        
instance Subtractive5 a1 a2 a3 a4 a5 => Subtractive (Product5 a1 a2 a3 a4 a5) where
    sub (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5)  
        = Product5 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5)
instance Subtractive6 a1 a2 a3 a4 a5 a6 => Subtractive (Product6 a1 a2 a3 a4 a5 a6) where
    sub (Product6 a1 a2 a3 a4 a5 a6) (Product6 b1 b2 b3 b4 b5 b6)  
        = Product6 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5) (a6 - b6)
instance Subtractive7 a1 a2 a3 a4 a5 a6 a7 => Subtractive (Product7 a1 a2 a3 a4 a5 a6 a7) where
    sub (Product7 a1 a2 a3 a4 a5 a6 a7) (Product7 b1 b2 b3 b4 b5 b6 b7)  
        = Product7 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5) (a6 - b6) (a7 - b7)
instance Subtractive8 a1 a2 a3 a4 a5 a6 a7 a8 => Subtractive (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    sub (Product8 a1 a2 a3 a4 a5 a6 a7 a8) (Product8 b1 b2 b3 b4 b5 b6 b7 b8)  
        = Product8 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5) (a6 - b6) (a7 - b7) (a8 - b8)
instance Subtractive9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Subtractive (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    sub (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) (Product9 b1 b2 b3 b4 b5 b6 b7 b8 b9)  
        = Product9 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5) (a6 - b6) (a7 - b7) (a8 - b8) (a9 - b9)

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
instance Reversible (Product8 a1 a2 a3 a4 a5 a6 a7 a8) (Product8 a8 a7 a6 a5 a4 a3 a2 a1) where
    reverse (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = Product8 a8 a7 a6 a5 a4 a3 a2 a1 
instance Reversible (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) (Product9 a9 a8 a7 a6 a5 a4 a3 a2 a1) where
    reverse (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Product9 a9 a8 a7 a6 a5 a4 a3 a2 a1 
        
type Nullary2 a1 a2 = (Nullary a1, Nullary a2)
type Nullary3 a1 a2 a3 = (Nullary2 a1 a2, Nullary a3)
type Nullary4 a1 a2 a3 a4 = (Nullary3 a1 a2 a3, Nullary a4)
type Nullary5 a1 a2 a3 a4 a5 = (Nullary4 a1 a2 a3 a4, Nullary a5)
type Nullary6 a1 a2 a3 a4 a5 a6 = (Nullary5 a1 a2 a3 a4 a5, Nullary a6)
type Nullary7 a1 a2 a3 a4 a5 a6 a7 = (Nullary6 a1 a2 a3 a4 a5 a6, Nullary a7)
type Nullary8 a1 a2 a3 a4 a5 a6 a7 a8 = (Nullary7 a1 a2 a3 a4 a5 a6 a7, Nullary a8)
type Nullary9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Nullary8 a1 a2 a3 a4 a5 a6 a7 a8, Nullary a9)
        
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

type Invertible2 a1 a2 = (Invertible a1 a1, Invertible a2 a2)
type Invertible3 a1 a2 a3 = (Invertible2 a1 a2, Invertible a3 a3)
type Invertible4 a1 a2 a3 a4 = (Invertible3 a1 a2 a3, Invertible a4 a4)
type Invertible5 a1 a2 a3 a4 a5 = (Invertible4 a1 a2 a3 a4, Invertible a5 a5)
type Invertible6 a1 a2 a3 a4 a5 a6 = (Invertible5 a1 a2 a3 a4 a5, Invertible a6 a6)
type Invertible7 a1 a2 a3 a4 a5 a6 a7 = (Invertible6 a1 a2 a3 a4 a5 a6, Invertible a7 a7)
type Invertible8 a1 a2 a3 a4 a5 a6 a7 a8 = (Invertible7 a1 a2 a3 a4 a5 a6 a7, Invertible a8 a8)
type Invertible9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Invertible8 a1 a2 a3 a4 a5 a6 a7 a8, Invertible a9 a9)
        
instance Invertible2 a1 a2 => Invertible (Product2 a1 a2) (Product2 a1 a2) where
    invert (Product2 a1 a2) 
        = Product2 (invert a1) (invert a2)    
instance Invertible3 a1 a2 a3 => Invertible (Product3 a1 a2 a3) (Product3 a1 a2 a3) where
    invert (Product3 a1 a2 a3) 
        = Product3 (invert a1) (invert a2) (invert a3)
instance Invertible4 a1 a2 a3 a4 => Invertible (Product4 a1 a2 a3 a4) (Product4 a1 a2 a3 a4) where
    invert (Product4 a1 a2 a3 a4) 
        = Product4 (invert a1) (invert a2) (invert a3) (invert a4)
instance Invertible5 a1 a2 a3 a4 a5 => Invertible (Product5 a1 a2 a3 a4 a5) (Product5 a1 a2 a3 a4 a5) where
    invert (Product5 a1 a2 a3 a4 a5) 
        = Product5 (invert a1) (invert a2) (invert a3) (invert a4) (invert a5)
instance Invertible6 a1 a2 a3 a4 a5 a6 => Invertible (Product6 a1 a2 a3 a4 a5 a6) (Product6 a1 a2 a3 a4 a5 a6) where
    invert (Product6 a1 a2 a3 a4 a5 a6) 
        = Product6 (invert a1) (invert a2) (invert a3) (invert a4) (invert a5) (invert a6)
instance Invertible7 a1 a2 a3 a4 a5 a6 a7 => Invertible (Product7 a1 a2 a3 a4 a5 a6 a7) (Product7 a1 a2 a3 a4 a5 a6 a7) where
    invert (Product7 a1 a2 a3 a4 a5 a6 a7) 
        = Product7 (invert a1) (invert a2) (invert a3) (invert a4) (invert a5) (invert a6) (invert a7)
instance Invertible8 a1 a2 a3 a4 a5 a6 a7 a8 => Invertible (Product8 a1 a2 a3 a4 a5 a6 a7 a8) (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    invert (Product8 a1 a2 a3 a4 a5 a6 a7 a8) 
        = Product8 (invert a1) (invert a2) (invert a3) (invert a4) (invert a5) (invert a6) (invert a7) (invert a8)
instance Invertible9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Invertible (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    invert (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) 
        = Product9 (invert a1) (invert a2) (invert a3) (invert a4) (invert a5) (invert a6) (invert a7) (invert a8) (invert a9)        

type Unital2 a1 a2 = (Unital a1, Unital a2)
type Unital3 a1 a2 a3 = (Unital2 a1 a2, Unital a3)
type Unital4 a1 a2 a3 a4 = (Unital3 a1 a2 a3, Unital a4)
type Unital5 a1 a2 a3 a4 a5 = (Unital4 a1 a2 a3 a4, Unital a5)
type Unital6 a1 a2 a3 a4 a5 a6 = (Unital5 a1 a2 a3 a4 a5, Unital a6)
type Unital7 a1 a2 a3 a4 a5 a6 a7 = (Unital6 a1 a2 a3 a4 a5 a6, Unital a7)
type Unital8 a1 a2 a3 a4 a5 a6 a7 a8 = (Unital7 a1 a2 a3 a4 a5 a6 a7, Unital a8)
type Unital9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Unital8 a1 a2 a3 a4 a5 a6 a7 a8, Unital a9)
    
instance Unital2 a1 a2 => Unital (Product2 a1 a2) where
    one = prod (one,one)
instance Unital3 a1 a2 a3 => Unital (Product3 a1 a2 a3) where
    one = prod (one,one,one)    
instance Unital4 a1 a2 a3 a4 => Unital (Product4 a1 a2 a3 a4) where
    one = prod (one,one,one,one)
instance Unital5 a1 a2 a3 a4 a5 => Unital (Product5 a1 a2 a3 a4 a5) where
    one = prod (one,one,one,one,one)
instance Unital6 a1 a2 a3 a4 a5 a6 => Unital (Product6 a1 a2 a3 a4 a5 a6) where
    one = prod (one,one,one,one,one,one)
instance Unital7 a1 a2 a3 a4 a5 a6 a7 => Unital (Product7 a1 a2 a3 a4 a5 a6 a7) where
    one = prod (one,one,one,one,one,one,one)
instance Unital8 a1 a2 a3 a4 a5 a6 a7 a8 => Unital (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    one = prod (one,one,one,one,one,one,one,one)
instance Unital9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Unital (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    one = prod (one,one,one,one,one,one,one,one,one)

type Semigroup2 a1 a2 = (Additive a1, Additive a2, Semigroup a1, Semigroup a2)
type Semigroup3 a1 a2 a3 = (Semigroup2 a1 a2, Additive a3, Semigroup a3)
type Semigroup4 a1 a2 a3 a4 = (Semigroup3 a1 a2 a3, Additive a4, Semigroup a4)
type Semigroup5 a1 a2 a3 a4 a5 = (Semigroup4 a1 a2 a3 a4, Additive a5, Semigroup a5)
type Semigroup6 a1 a2 a3 a4 a5 a6 = (Semigroup5 a1 a2 a3 a4 a5, Additive a6, Semigroup a6)
type Semigroup7 a1 a2 a3 a4 a5 a6 a7 = (Semigroup6 a1 a2 a3 a4 a5 a6, Additive a7, Semigroup a7)
type Semigroup8 a1 a2 a3 a4 a5 a6 a7 a8 = (Semigroup7 a1 a2 a3 a4 a5 a6 a7, Additive a8, Semigroup a8)
type Semigroup9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Semigroup8 a1 a2 a3 a4 a5 a6 a7 a8, Additive a9, Semigroup a9)

instance Semigroup2 a1 a2 => Semigroup (Product2 a1 a2) where (<>) x y = x + y
instance Semigroup3 a1 a2 a3 => Semigroup (Product3 a1 a2 a3) where (<>) x y = x + y
instance Semigroup4 a1 a2 a3 a4 => Semigroup (Product4 a1 a2 a3 a4) where (<>) x y = x + y
instance Semigroup5 a1 a2 a3 a4 a5 => Semigroup (Product5 a1 a2 a3 a4 a5) where (<>) x y = x + y
instance Semigroup6 a1 a2 a3 a4 a5 a6 => Semigroup (Product6 a1 a2 a3 a4 a5 a6) where (<>) x y = x + y
instance Semigroup7 a1 a2 a3 a4 a5 a6 a7 => Semigroup (Product7 a1 a2 a3 a4 a5 a6 a7) where (<>) x y = x + y
instance Semigroup8 a1 a2 a3 a4 a5 a6 a7 a8 => Semigroup (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where (<>) x y = x + y
instance Semigroup9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Semigroup (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where (<>) x y = x + y

type Monoidal a = (Monoid a, Nullary a, Additive a)
type Monoid2 a1 a2 = (Monoid a1, Monoid a2, Nullary2 a1 a2, Additive2 a1 a2)
type Monoid3 a1 a2 a3 = (Monoid2 a1 a2, Nullary a3, Monoid a3, Additive a3)
type Monoid4 a1 a2 a3 a4 = (Monoid3 a1 a2 a3, Nullary a4, Monoid a4, Additive a4)
type Monoid5 a1 a2 a3 a4 a5 = (Monoid4 a1 a2 a3 a4, Nullary a5, Monoid a5, Additive a5)
type Monoid6 a1 a2 a3 a4 a5 a6 = (Monoid5 a1 a2 a3 a4 a5, Nullary a6, Monoid a6, Additive a6)
type Monoid7 a1 a2 a3 a4 a5 a6 a7 = (Monoid6 a1 a2 a3 a4 a5 a6, Nullary a7, Monoid a7, Additive a7)
type Monoid8 a1 a2 a3 a4 a5 a6 a7 a8 = (Monoid7 a1 a2 a3 a4 a5 a6 a7, Nullary a8, Monoid a8, Additive a8)
type Monoid9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Monoid8 a1 a2 a3 a4 a5 a6 a7 a8, Nullary a9,Monoid a9, Additive a9)

instance Monoid2 a1 a2 => Monoid (Product2 a1 a2) where mempty = zero
instance Monoid3 a1 a2 a3 => Monoid (Product3 a1 a2 a3) where mempty = zero
instance Monoid4 a1 a2 a3 a4 => Monoid (Product4 a1 a2 a3 a4) where mempty = zero
instance Monoid5 a1 a2 a3 a4 a5 => Monoid (Product5 a1 a2 a3 a4 a5) where mempty = zero
instance Monoid6 a1 a2 a3 a4 a5 a6 => Monoid (Product6 a1 a2 a3 a4 a5 a6) where mempty = zero
instance Monoid7 a1 a2 a3 a4 a5 a6 a7 => Monoid (Product7 a1 a2 a3 a4 a5 a6 a7) where mempty = zero
instance Monoid8 a1 a2 a3 a4 a5 a6 a7 a8 => Monoid (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where mempty = zero
instance Monoid9 a1 a2 a3 a4 a5 a6 a7 a8 a9 => Monoid (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where mempty = zero

type Group2 a1 a2 = (Group a1, Group a2, Monoidal a1, Monoidal a2)
type Group3 a1 a2 a3 = (Group2 a1 a2, Group a3, Monoidal a3)
type Group4 a1 a2 a3 a4 = (Group3 a1 a2 a3, Group a4, Monoidal a4)
type Group5 a1 a2 a3 a4 a5 = (Group4 a1 a2 a3 a4, Group a5, Monoidal a5)
type Group6 a1 a2 a3 a4 a5 a6 = (Group5 a1 a2 a3 a4 a5, Group a6, Monoidal a6)
type Group7 a1 a2 a3 a4 a5 a6 a7 = (Group6 a1 a2 a3 a4 a5 a6, Group a7, Monoidal a7)
type Group8 a1 a2 a3 a4 a5 a6 a7 a8 = (Group7 a1 a2 a3 a4 a5 a6 a7, Group a8, Monoidal a8)
type Group9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = (Group8 a1 a2 a3 a4 a5 a6 a7 a8, Group a9, Monoidal a9)
    
--instance Group2 a1 a2 => Group (Product2 a1 a2)
