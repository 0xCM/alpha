-----------------------------------------------------------------------------
-- | Defines abstractions over functions
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Data.Func
(
    Func0, Func1, Func2, Func3, Func4, Func5, Func6, Func7, Func8, Func9,
    CFunc1, CFunc2, CFunc3, CFunc4, CFunc5, CFunc6,
    SFunc1, SFunc2, SFunc3, SFunc4, SFunc5,

    Curried, Uncurried, curry, uncurry,
    
    CFunc(..),
    SFunc(..),
    Func(..), Functional,
    NFunc(..), NFx(..)

)
where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product
import Alpha.Data.Sum
import qualified Data.Tuple as Tuple


instance Compositional (Product2 b c) (Product2 a b) (Product2 a c)        

-- Synonym for an 'uncurried' function
type Uncurried a b c = Function (a, b) c

-- Synonym for an 'curried' function
type Curried a b c =  a -> b -> c

curry::Uncurried a b c -> Curried a b c
curry = Tuple.curry

uncurry::Curried a b c -> Uncurried a b c
uncurry = Tuple.uncurry

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
type family Func a r where
    Func (Product1 a1) r  = Func1 a1 r
    Func (Product2 a1 a2) r = Func2 a1 a2 r
    Func (Product3 a1 a2 a3) r = Func3 a1 a2 a3 r
    Func (Product4 a1 a2 a3 a4) r = Func4 a1 a2 a3 a4 r
    Func (Product5 a1 a2 a3 a4 a5) r= Func5 a1 a2 a3 a4 a5 r
    Func (Product6 a1 a2 a3 a4 a5 a6) r = Func6 a1 a2 a3 a4 a5 a6 r
    Func (Product7 a1 a2 a3 a4 a5 a6 a7) r = Func7 a1 a2 a3 a4 a5 a6 a7 r
    Func (Product8 a1 a2 a3 a4 a5 a6 a7 a8) r = Func8 a1 a2 a3 a4 a5 a6 a7 a8 r
    Func (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r = Func9 a1 a2 a3 a4 a5 a6 a7 a8 a9 r

-- Synonym for a 1-component function        
type CFunc1 a1 b1
    =  Product1 (Func1 a1 b1)
-- Synonym for a 2-component function            
type CFunc2 a1 b1 a2 b2 
    =  Product2 (Func1 a1 b1) (Func1 a2 b2)
-- Synonym for a 3-component function                
type CFunc3 a1 b1 a2 b2 a3 b3 
    = Product3 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3)
-- Synonym for a 4-component function                
type CFunc4 a1 b1 a2 b2 a3 b3 a4 b4 
    = Product4 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4)
-- Synonym for a 5-component function                
type CFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 
    = Product5 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5)
-- Synonym for a 6-component function                
type CFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 
    = Product6 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5) (Func1 a6 b6)
-- Synonym for a 7-component function                
type CFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7
    = Product7 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5) (Func1 a6 b6) (Func1 a7 b7)

-- Defines a family of functions defined component-wise    
type family CFunc a | a -> a where
    CFunc (CFunc1 a1 b1) 
        = CFunc1 a1 b1
    CFunc (CFunc2 a1 b1 a2 b2) 
        = CFunc2 a1 b1 a2 b2
    CFunc (CFunc3 a1 b1 a2 b2 a3 b3) 
        = CFunc3 a1 b1 a2 b2 a3 b3
    CFunc (CFunc4 a1 b1 a2 b2 a3 b3 a4 b4) 
        = CFunc4 a1 b1 a2 b2 a3 b3 a4 b4
    CFunc (CFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5) 
        = CFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5
    CFunc (CFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6) 
        = CFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6
    CFunc (CFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7) 
        = CFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7
    
type family NFunc a b where
    NFunc (Product2 a1 a2) (Product2 b1 b2) 
        = Product2 (Func1 a1 b1) (Func1 a2 b2)
    NFunc (Product3 a1 a2 a3) (Product3 b1 b2 b3) 
        = Product3 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3)
    NFunc (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) 
        = Product4 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4)
    NFunc (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) 
        = Product5 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5)
    NFunc (Product6 a1 a2 a3 a4 a5 a6) (Product6 b1 b2 b3 b4 b5 b6) 
        = Product6 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5) (Func1 a6 b6)
    NFunc (Product7 a1 a2 a3 a4 a5 a6 a7) (Product7 b1 b2 b3 b4 b5 b6 b7) 
        = Product7 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5) (Func1 a6 b6) (Func1 a7 b7)

-- Synononym for a sum-function of arity 1
type SFunc1 a1 b1 
    = Func1 (CFunc1 a1 b1) (Sum1 b1)    
-- Synononym for a sum-function of arity 2    
type SFunc2 a1 b1 a2 b2 
    = Func1 (CFunc2 a1 b1 a2 b2) (Sum2 b1 b2)
-- Synononym for a sum-function of arity 3    
type SFunc3 a1 b1 a2 b2 a3 b3
    = Func1 (CFunc3 a1 b1 a2 b2 a3 b3) (Sum3 b1 b2 b3)
-- Synononym for a sum-function of arity 4    
type SFunc4 a1 b1 a2 b2 a3 b3 a4 b4
    = Func1 (CFunc4 a1 b1 a2 b2 a3 b3 a4 b4) (Sum4 b1 b2 b3 b4)
-- Synononym for a sum-function of arity 5    
type SFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5
    = Func1 (CFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5) (Sum5 b1 b2 b3 b4 b5)
-- Synononym for a sum-function of arity 6
type SFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6
    = Func1 (CFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6) (Sum6 b1 b2 b3 b4 b5 b6)
-- Synononym for a sum-function of arity 6
type SFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7
    = Func1 (CFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7) (Sum7 b1 b2 b3 b4 b5 b6 b7)

-- Defines family of function that accept and produce sums
type family SFunc a b where
    SFunc (Sum2 a1 a2) (Sum2 b1 b2) 
        = SFunc2 a1 b1 a2 b2
    SFunc (Sum3 a1 a2 a3) (Sum3 b1 b2 b3)        
        = SFunc3 a1 b1 a2 b3 a3 b3
    SFunc (Sum4 a1 a2 a3 a4) (Sum4 b1 b2 b3 b4)        
        = SFunc4 a1 b1 a2 b3 a3 b3 a4 b4
    SFunc (Sum5 a1 a2 a3 a4 a5) (Sum5 b1 b2 b3 b4 b5)        
        = SFunc5 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5
    SFunc (Sum6 a1 a2 a3 a4 a5 a6) (Sum6 b1 b2 b3 b4 b5 b6)        
        = SFunc6 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6
    SFunc (Sum7 a1 a2 a3 a4 a5 a6 a7) (Sum7 b1 b2 b3 b4 b5 b6 b7)        
        = SFunc7 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7


class Functional a r where    

    type F a r    
    type F a r = Func a r
    
    eval::F a r -> a -> r

    func::(f ~ F a r) => f -> F a r
    func f = f    
    
instance Functional (Product1 a1) r where
    eval f (Product1 a1) = f a1                
instance Functional (Product2 a1 a2) r where
    eval f (Product2 a1 a2) = f a1 a2    
instance Functional (Product3 a1 a2 a3) r where
    eval f (Product3 a1 a2 a3) = f a1 a2 a3    
instance Functional (Product4 a1 a2 a3 a4) r where
    eval f (Product4 a1 a2 a3 a4) = f a1 a2 a3 a4        
instance Functional (Product5 a1 a2 a3 a4 a5) r where
    eval f (Product5 a1 a2 a3 a4 a5) = f a1 a2 a3 a4 a5    
instance Functional (Product6 a1 a2 a3 a4 a5 a6) r where
    eval f (Product6 a1 a2 a3 a4 a5 a6) = f a1 a2 a3 a4 a5 a6
instance Functional (Product7 a1 a2 a3 a4 a5 a6 a7) r where
    eval f (Product7 a1 a2 a3 a4 a5 a6 a7) = f a1 a2 a3 a4 a5 a6 a7
                    
-- Defines constraints over coordinate functions
class NFx a b c where
    ncompose::NFunc b c -> NFunc a b -> NFunc a c

instance NFx (Product2 a1 a2) (Product2 b1 b2) (Product2 c1 c2) where
    ncompose (Product2 g1 g2) (Product2 f1 f2) 
        = Product2 (g1 . f1) (g2 . f2)
instance NFx (Product3 a1 a2 a3) (Product3 b1 b2 b3) (Product3 c1 c2 c3) where
    ncompose (Product3 g1 g2 g3) (Product3 f1 f2 f3) 
        = Product3 (g1 . f1) (g2 . f2) (g3 . f3)    
instance NFx (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) (Product4 c1 c2 c3 c4) where
    ncompose (Product4 g1 g2 g3 g4) (Product4 f1 f2 f3 f4) 
        = Product4 (g1 . f1) (g2 . f2) (g3 . f3) (g4 . f4)    
instance NFx (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) (Product5 c1 c2 c3 c4 c5) where
    ncompose (Product5 g1 g2 g3 g4 g5) (Product5 f1 f2 f3 f4 f5) 
        = Product5 (g1 . f1) (g2 . f2) (g3 . f3) (g4 . f4) (g5 . f5)
instance NFx (Product6 a1 a2 a3 a4 a5 a6) (Product6 b1 b2 b3 b4 b5 b6) (Product6 c1 c2 c3 c4 c5 c6) where
    ncompose (Product6 g1 g2 g3 g4 g5 g6) (Product6 f1 f2 f3 f4 f5 f6) 
        = Product6 (g1 . f1) (g2 . f2) (g3 . f3) (g4 . f4) (g5 . f5) (g6 . f6)


