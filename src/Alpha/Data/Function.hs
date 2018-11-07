module Alpha.Data.Function
(
    Func(..), Fx(..),
    NFunc(..), NFx(..)
    
)
where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product

type family Func a b where
    Func (Product1 a1) r  = Func1 a1 r
    Func (Product2 a1 a2) r = Func2 a1 a2 r
    Func (Product3 a1 a2 a3) r = Func3 a1 a2 a3 r
    Func (Product4 a1 a2 a3 a4) r = Func4 a1 a2 a3 a4 r
    Func (Product5 a1 a2 a3 a4 a5) r= Func5 a1 a2 a3 a4 a5 r
    Func (Product6 a1 a2 a3 a4 a5 a6) r = Func6 a1 a2 a3 a4 a5 a6 r
    Func (Product7 a1 a2 a3 a4 a5 a6 a7) r = Func7 a1 a2 a3 a4 a5 a6 a7 r

-- A function that takes between 0 and n arguments
data FxRankN f r = 
    Func0 (Func0 r) | forall a1.             
    Func1 (Func1 a1 r) | forall a1 a2.          
    Func2 (Func2 a1 a2 r) | forall a1 a2 a3.       
    Func3 (Func3 a1 a2 a3 r) | forall a1 a2 a3 a4.    
    Func4 (Func4 a1 a2 a3 a4 r) | forall a1 a2 a3 a4 a5. 
    Func5 (Func5 a1 a2 a3 a4 a5 r) | forall a1 a2 a3 a4 a5 a6. 
    Func6 (Func6 a1 a2 a3 a4 a5 a6 r)
    deriving (Typeable)
    
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
    
-- Defines the type of a product function
type family NFunc a r where
    NFunc (Product2 a1 a2) (Product2 b1 b2) 
        = Product2 (Func1 a1 b1) (Func1 a2 b2)

    NFunc (Product3 a1 a2 a3) (Product3 b1 b2 b3) 
        = Product3 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3)

    NFunc (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) 
        = Product4 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4)

    NFunc (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) 
        = Product5 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5)

-- Characterizes a product function evaluator        
class NFx a b where
    -- |Evaluates a product function
    pmap::NFunc a b -> a -> b
    
instance NFx (Product2 a1 a2) (Product2 b1 b2) where
    pmap (Product2 f1 f2) (Product2 a1 a2) 
        = Product2 (f1 a1) (f2 a2)
    
instance NFx (Product3 a1 a2 a3) (Product3 b1 b2 b3) where
    pmap (Product3 f1 f2 f3) (Product3 a1 a2 a3) 
        = Product3 (f1 a1) (f2 a2) (f3 a3)
    
instance NFx (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) where
    pmap (Product4 f1 f2 f3 f4) (Product4 a1 a2 a3 a4) 
        = Product4 (f1 a1) (f2 a2) (f3 a3) (f4 a4)
    
instance NFx (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) where
    pmap (Product5 f1 f2 f3 f4 f5) (Product5 a1 a2 a3 a4 a5) 
        = Product5 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)
    
