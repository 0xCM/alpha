{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Func
(
    Func0, Func1,Func2,Func3,Func4,Func5,Func6,Func7,Func8,Func9,
    Func(..), Fx(..)    
)
where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product

-- A synonym for a constant function
type Func0 r  = r

-- A synonym for a function that saturates with 1 argument
type Func1 a1 r = a1 -> r

-- A synonym for a function that saturates with 2 arguments
type Func2 a1 a2 r = a1 -> a2 -> r

-- A synonym for a function that saturates with 3 arguments
type Func3 a1 a2 a3 r = a1 -> a2 -> a3 -> r

-- A synonym for a function that saturates with 4 arguments
type Func4 a1 a2 a3 a4 r = a1 -> a2 -> a3 -> a4 -> r

-- A synonym for a function that saturates with 5 arguments
type Func5 a1 a2 a3 a4 a5 r = a1 -> a2 -> a3 -> a4 -> a5 -> r

-- A synonym for a function that saturates with 6 arguments
type Func6 a1 a2 a3 a4 a5 a6 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r

-- A synonym for a function that saturates with 7 arguments
type Func7 a1 a2 a3 a4 a5 a6 a7 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r

-- A synonym for a function that saturates with 8 arguments
type Func8 a1 a2 a3 a4 a5 a6 a7 a8 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r

-- A synonym for a function that saturates with 9 arguments
type Func9 a1 a2 a3 a4 a5 a6 a7 a8 a9 r = a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r

-- Defines family of canonical function types
type family Func a b where
    Func (Product1 a1) r  = Func1 a1 r
    Func (Product2 a1 a2) r = Func2 a1 a2 r
    Func (Product3 a1 a2 a3) r = Func3 a1 a2 a3 r
    Func (Product4 a1 a2 a3 a4) r = Func4 a1 a2 a3 a4 r
    Func (Product5 a1 a2 a3 a4 a5) r= Func5 a1 a2 a3 a4 a5 r
    Func (Product6 a1 a2 a3 a4 a5 a6) r = Func6 a1 a2 a3 a4 a5 a6 r
    Func (Product7 a1 a2 a3 a4 a5 a6 a7) r = Func7 a1 a2 a3 a4 a5 a6 a7 r
    Func (Product8 a1 a2 a3 a4 a5 a6 a7 a8) r = Func8 a1 a2 a3 a4 a5 a6 a7 a8 r
    Func (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r = Func9 a1 a2 a3 a4 a5 a6 a7 a8 a9 r
    
class Fx a r where
    eval::Func a r -> a -> r    

    func::(f ~ Func a r) => f -> Func a r
    func f = f    
    
instance Fx (Product1 a1) r where
    eval f (Product1 a1) = f a1
                
instance Fx (Product2 a1 a2) r where
    eval f (Product2 a1 a2) = f a1 a2
    
instance Fx (Product3 a1 a2 a3) r where
    eval f (Product3 a1 a2 a3) = f a1 a2 a3
    
instance Fx (Product4 a1 a2 a3 a4) r where
    eval f (Product4 a1 a2 a3 a4) = f a1 a2 a3 a4    
    
instance Fx (Product5 a1 a2 a3 a4 a5) r where
    eval f (Product5 a1 a2 a3 a4 a5) = f a1 a2 a3 a4 a5
    
instance Fx (Product6 a1 a2 a3 a4 a5 a6) r where
    eval f (Product6 a1 a2 a3 a4 a5 a6) = f a1 a2 a3 a4 a5 a6

instance Fx (Product7 a1 a2 a3 a4 a5 a6 a7) r where
    eval f (Product7 a1 a2 a3 a4 a5 a6 a7) = f a1 a2 a3 a4 a5 a6 a7
            