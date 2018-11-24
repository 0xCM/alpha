{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Alpha.Data.Product
(
    Product, UniProduct, 
    Productive(..), Factored(..),
    
    Product1(..), Product2(..), Product3(..), Product4(..), Product5(..),
    Product6(..), Product7(..), Product8(..), Product9(..),

    Pair,Triple,

    UniProduct1(..), UniProduct2(..), UniProduct3(..), UniProduct4(..), UniProduct5(..),
    UniProduct6(..), UniProduct7(..), UniProduct8(..), UniProduct9(..)


    
) where
import Alpha.Base hiding(zero)
import Alpha.Canonical
import Alpha.Text.Combinators
import qualified Control.Category as C
import qualified Data.Vector as V

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

-- Defines an arity-1 heterogenous product
data Product1 a1 
    = Product1 !a1
        deriving (Eq, Ord, Data, Generic, Typeable, Show, Read)

-- Defines an arity-1 homogenous product
type UniProduct1 a = Product1 a

-- Defines an arity-2 heterogenous product
data Product2 a1 a2 
    = Product2 !a1 !a2
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- | Synonym for a product of arity 2                
type Pair a1 a2 = Product2 a1 a2        

-- Defines an arity-2 homogenous product        
type UniProduct2 a = Product2 a a

-- Defines an arity-3 heterogenous product
data Product3 a1 a2 a3 
    = Product3 !a1 !a2 !a3
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- | Synonym for a product of arity 3        
type Triple a1 a2 a3 = Product3 a1 a2 a3

-- Defines an arity-3 homogenous product        
type UniProduct3 a = Product3 a a a

-- Defines an arity-4 heterogenous product
data Product4 a1 a2 a3 a4 
    = Product4 !a1 !a2 !a3 !a4
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- Defines an arity-4 homogenous product        
type UniProduct4 a = Product4 a a a a

-- Defines an arity-5 heterogenous product        
data Product5 a1 a2 a3 a4 a5 
    = Product5 !a1 !a2 !a3 !a4 !a5
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- Defines an arity-5 homogenous product        
type UniProduct5 a = Product5 a a a a a

-- Defines an arity-6 heterogenous product        
data Product6 a1 a2 a3 a4 a5 a6 
    = Product6 !a1 !a2 !a3 !a4 !a5 !a6
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- Defines an arity-6 homogenous product        
type UniProduct6 a = Product6 a a a a a a

-- Defines an arity-7 heterogenous product
data Product7 a1 a2 a3 a4 a5 a6 a7 
    = Product7 !a1 !a2 !a3 !a4 !a5 !a6 !a7
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- Defines an arity-7 homogenous product        
type UniProduct7 a = Product7 a a a a a a a

-- Defines an arity-8 heterogenous product        
data Product8 a1 a2 a3 a4 a5 a6 a7 a8 
    = Product8 !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8
        deriving (Eq, Ord, Data,Generic, Typeable, Show, Read)

-- Defines an arity-8 homogenous product        
type UniProduct8 a = Product8 a a a a a a a a

-- Defines an arity-9 heterogenous product        
data Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9 
    = Product9 !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8 !a9
        deriving (Eq, Ord, Data,Generic, Typeable, Show)

-- Defines an arity-9 homogenous product        
type UniProduct9 a = Product9 a a a a a a a a a

        
-- Unifies arity-specific product definitions
type family Product a = r | r -> a where
    Product (a1,a2) = Product2 a1 a2
    Product (a1,a2,a3) = Product3 a1 a2 a3
    Product (a1,a2,a3,a4) = Product4 a1 a2 a3 a4
    Product (a1,a2,a3,a4,a5)  = Product5 a1 a2 a3 a4 a5
    Product (a1,a2,a3,a4,a5,a6)  = Product6 a1 a2 a3 a4 a5 a6 
    Product (a1,a2,a3,a4,a5,a6,a7)  = Product7 a1 a2 a3 a4 a5 a6 a7
    Product (a1,a2,a3,a4,a5,a6,a7,a8)  = Product8 a1 a2 a3 a4 a5 a6 a7 a8
    Product (a1,a2,a3,a4,a5,a6,a7,a8,a9)  = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9

type a !*! b = Product2 a b

-- Constructs a product
(!*!)::a -> b -> a !*! b
(!*!) = Product2
infixl 5 !*!

type family a !~! b  where
    (Product2 a1 a2) !~! (Product2 a3 a4) = Product4 a1 a2 a3 a4
    (Product2 a1 a2) !~! (Product3 a3 a4 a5) = Product5 a1 a2 a3 a4 a5
    (Product2 a1 a2) !~! (Product4 a3 a4 a5 a6) = Product6 a1 a2 a3 a4 a5 a6
    (Product2 a1 a2) !~! (Product5 a3 a4 a5 a6 a7) = Product7 a1 a2 a3 a4 a5 a6 a7
    (Product2 a1 a2) !~! (Product6 a3 a4 a5 a6 a7 a8) = Product8 a1 a2 a3 a4 a5 a6 a7 a8
    (Product2 a1 a2) !~! (Product7 a3 a4 a5 a6 a7 a8 a9) = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    (Product3 a1 a2 a3) !~! (Product2 a4 a5) = Product5 a1 a2 a3 a4 a5
    (Product3 a1 a2 a3) !~! (Product3 a4 a5 a6) = Product6 a1 a2 a3 a4 a5 a6
    (Product3 a1 a2 a3) !~! (Product4 a4 a5 a6 a7) = Product7 a1 a2 a3 a4 a5 a6 a7
    (Product3 a1 a2 a3) !~! (Product5 a4 a5 a6 a7 a8) = Product8 a1 a2 a3 a4 a5 a6 a7 a8
    (Product3 a1 a2 a3) !~! (Product6 a4 a5 a6 a7 a8 a9) = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9

-- Defines a family of uniform products, i.e., products where factors are homogenous     
type family UniProduct (n::Nat) a = r | r -> n a where
    UniProduct 2 (a,a) = UniProduct2 a
    UniProduct 3 (a,a,a) = UniProduct3 a
    UniProduct 4 (a,a,a,a) = UniProduct4 a
    UniProduct 5 (a,a,a,a,a)  = UniProduct5 a
    UniProduct 6 (a,a,a,a,a,a)  = UniProduct6 a
    UniProduct 7 (a,a,a,a,a,a,a)  = UniProduct7 a
    UniProduct 8 (a,a,a,a,a,a,a,a)  = UniProduct8 a
    UniProduct 9 (a,a,a,a,a,a,a,a,a)  = UniProduct9 a


-- Characterizes types from which products can be constructed
class  Productive a where    
    -- | Forms a product from an input type
    prod::a -> Product a

    
instance Productive (a1,a2) where
    prod (a1,a2) = Product2 a1 a2
instance Productive (a1,a2,a3) where
    prod (a1,a2,a3) = Product3 a1 a2 a3    
instance Productive (a1,a2,a3,a4)  where
    prod (a1,a2,a3,a4) = Product4 a1 a2 a3 a4    
instance Productive (a1,a2,a3,a4,a5) where
    prod (a1,a2,a3,a4,a5) = Product5 a1 a2 a3 a4 a5    
instance Productive (a1,a2,a3,a4,a5,a6) where
    prod (a1,a2,a3,a4,a5,a6) = Product6 a1 a2 a3 a4 a5 a6    
instance Productive (a1,a2,a3,a4,a5,a6,a7) where
    prod (a1,a2,a3,a4,a5,a6,a7) = Product7 a1 a2 a3 a4 a5 a6 a7    
instance Productive (a1,a2,a3,a4,a5,a6,a7,a8) where
    prod (a1,a2,a3,a4,a5,a6,a7,a8) = Product8 a1 a2 a3 a4 a5 a6 a7 a8    
instance Productive (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    prod (a1,a2,a3,a4,a5,a6,a7,a8,a9) = Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    
instance Tupled 2 (Product2 a1 a2) (a1,a2) where
    tuple (Product2 a1 a2) = (a1,a2)     
instance Tupled 3 (Product3 a1 a2 a3) (a1,a2,a3) where
    tuple (Product3 a1 a2 a3) = (a1,a2, a3) 
instance Tupled 4 (Product4 a1 a2 a3 a4) (a1,a2,a3,a4) where
    tuple (Product4 a1 a2 a3 a4) = (a1,a2,a3,a4) 
instance Tupled 5 (Product5 a1 a2 a3 a4 a5) (a1,a2,a3,a4,a5) where
    tuple (Product5 a1 a2 a3 a4 a5) = (a1,a2,a3,a4,a5)         
instance Tupled 6 (Product6 a1 a2 a3 a4 a5 a6) (a1,a2,a3,a4,a5,a6) where
    tuple (Product6 a1 a2 a3 a4 a5 a6) = (a1,a2,a3,a4,a5,a6)         
instance Tupled 7 (Product7 a1 a2 a3 a4 a5 a6 a7) (a1,a2,a3,a4,a5,a6,a7) where
    tuple (Product7 a1 a2 a3 a4 a5 a6 a7) = (a1,a2,a3,a4,a5,a6,a7)
instance Tupled 8 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tuple (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance Tupled 9 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tuple (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)            

-- Characterizes a componentized type a with n components indexed by j
class Factored (j::Nat) a where    
    -- | Extracts the j-th component from a product
    project::a -> Factor j a

instance Factored 1 (Product2 a1 a2) where 
    project (Product2 a1 a2) = a1    
instance Factored 2 (Product2 a1 a2) where 
    project (Product2 a1 a2) = a2
    
instance Factored 1 (Product3 a1 a2 a3) where 
    project (Product3 a1 a2 a3) = a1
instance Factored 2 (Product3 a1 a2 a3) where 
    project (Product3 a1 a2 a3) = a2
instance Factored 3 (Product3 a1 a2 a3) where 
    project (Product3 a1 a2 a3) = a3

instance Factored 1 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a1
instance Factored 2 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a2
instance Factored 3 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a3    
instance Factored 4 (Product4 a1 a2 a3 a4) where 
    project (Product4 a1 a2 a3 a4) = a4

instance Factored 1 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 a2 a3 a4 a5) = a1
instance Factored 2 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 a2 a3 a4 a5) = a2
instance Factored 3 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 a2 a3 a4 a5) = a3    
instance Factored 4 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 a2 a3 a4 a5) = a4
instance Factored 5 (Product5 a1 a2 a3 a4 a5) where 
    project (Product5 a1 a2 a3 a4 a5) = a5

instance Factored 1 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a1
instance Factored 2 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a2
instance Factored 3 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a3    
instance Factored 4 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a4
instance Factored 5 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a5
instance Factored 6 (Product6 a1 a2 a3 a4 a5 a6) where 
    project (Product6 a1 a2 a3 a4 a5 a6) = a6

instance Factored 1 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a1
instance Factored 2 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a2
instance Factored 3 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a3    
instance Factored 4 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a4
instance Factored 5 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a5
instance Factored 6 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a6
instance Factored 7 (Product7 a1 a2 a3 a4 a5 a6 a7) where 
    project (Product7 a1 a2 a3 a4 a5 a6 a7) = a7

instance Factored 1 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a1
instance Factored 2 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a2
instance Factored 3 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a3    
instance Factored 4 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a4
instance Factored 5 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a5
instance Factored 6 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a6
instance Factored 7 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a7
instance Factored 8 (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    project (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = a8

instance Factored 1 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a1
instance Factored 2 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a2
instance Factored 3 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a3    
instance Factored 4 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a4
instance Factored 5 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a5
instance Factored 6 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a6
instance Factored 7 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a7
instance Factored 8 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a8
instance Factored 9 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    project (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a9
                
instance Arital 2 (Product2 a1 a2)
instance Arital 3 (Product3 a1 a2 a3)
instance Arital 4 (Product4 a1 a2 a3 a4)
instance Arital 5 (Product5 a1 a2 a3 a4 a5)
instance Arital 6 (Product6 a1 a2 a3 a4 a5 a6)
instance Arital 7 (Product7 a1 a2 a3 a4 a5 a6 a7)
instance Arital 8 (Product8 a1 a2 a3 a4 a5 a6 a7 a8)
instance Arital 9 (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    
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

instance Functor Product1 where
    fmap f ~ (Product1 x) = Product1 (f x)
instance Functor (Product2 a1) where
    fmap f ~ (Product2 a1 x) = Product2 a1 (f x)
instance Functor (Product3 a1 a2) where
    fmap f ~ (Product3 a1 a2 x) = Product3 a1 a2 (f x)
instance Functor (Product4 a1 a2 a3) where
    fmap f ~ (Product4 a1 a2 a3 x) = Product4 a1 a2 a3 (f x)
instance Functor (Product5 a1 a2 a3 a4) where
    fmap f ~ (Product5 a1 a2 a3 a4 x) = Product5 a1 a2 a3 a4 (f x)
instance Functor (Product6 a1 a2 a3 a4 a5) where
    fmap f ~ (Product6 a1 a2 a3 a4 a5 x) = Product6 a1 a2 a3 a4 a5 (f x)
instance Functor (Product7 a1 a2 a3 a4 a5 a6) where
    fmap f ~ (Product7 a1 a2 a3 a4 a5 a6 x) = Product7 a1 a2 a3 a4 a5 a6 (f x)
        
instance Foldable (Product2 a) where
    foldMap f (Product2 _ b) = f b
    foldr f c (Product2 _ b) = f b c
    
instance Traversable (Product2 a) where
    traverse f (Product2 a b) = Product2 a <$> f b
    
    
instance Bifunctor Product2 where
    bimap f g ~ (Product2 a1 a2) = Product2 (f a1) (g a2)
instance Bifunctor (Product3 a1) where
    bimap f g ~ (Product3 a1 x y) = Product3 a1 (f x) (g y)
instance Bifunctor (Product4 a1 a2) where
    bimap f g ~ (Product4 a1 a2 x y) = Product4 a1 a2 (f x) (g y)
instance Bifunctor (Product5 a1 a2 a3) where
    bimap f g ~ (Product5 a1 a2 a3 x y) = Product5 a1 a2 a3 (f x) (g y)
instance Bifunctor (Product6 a1 a2 a3 a4) where
    bimap f g ~ (Product6 a1 a2 a3 a4 x y) = Product6 a1 a2 a3 a4 (f x) (g y)
instance Bifunctor (Product7 a1 a2 a3 a4 a5) where
    bimap f g ~ (Product7 a1 a2 a3 a4 a5 x y) = Product7 a1 a2 a3 a4 a5 (f x) (g y)
            
instance Biapply Product2 where
    Product2 f g <<.>> Product2 a b = Product2 (f a) (g b)

instance Biapplicative Product2 where
    bipure = Product2        
    Product2 f g <<*>> Product2 x y = Product2 (f x) (g y)    
    biliftA2 f g (Product2 x y) (Product2 a b) = Product2 (f x a) (g y b)
    
instance Monoid a1 => Biapplicative (Product3 a1)  where
    bipure = Product3 mempty
    Product3 a1 f g <<*>> Product3 a1' x y = Product3 (mappend a1 a1') (f x) (g y)
    
instance Monoid2 a1 a2 => Biapplicative (Product4 a1 a2)  where
    bipure a1 a2 = Product4 mempty mempty a1 a2
    Product4 a1 a2 f g <<*>> Product4 a1' a2' x y = Product4 (mappend a1 a1') (mappend a2 a2') (f x) (g y)
          
instance Comonad (Product2 e) where
    duplicate src = Product2 (project @1 src) src
    extract src = project @2 src


