module Alpha.Data.NFunc
(
    NFunc(..), NFx(..)    
) where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product
import Alpha.Data.Func

-- Defines a family of coordinate function types
type family NFunc a b where
    NFunc (Product2 a1 a2) (Product2 b1 b2) 
        = Product2 (Func1 a1 b1) (Func1 a2 b2)

    NFunc (Product3 a1 a2 a3) (Product3 b1 b2 b3) 
        = Product3 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3)

    NFunc (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) 
        = Product4 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4)

    NFunc (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) 
        = Product5 (Func1 a1 b1) (Func1 a2 b2) (Func1 a3 b3) (Func1 a4 b4) (Func1 a5 b5)

-- Defines constraints over coordinate functions
class NFx a b c where
    ncompose::NFunc b c -> NFunc a b -> NFunc a c

instance NFx (Product2 a1 a2) (Product2 b1 b2) (Product2 c1 c2) where
    ncompose (Product2 g1 g2) (Product2 f1 f2) = Product2 (g1 . f1) (g2 . f2)

instance NFx (Product3 a1 a2 a3) (Product3 b1 b2 b3) (Product3 c1 c2 c3) where
    ncompose (Product3 g1 g2 g3) (Product3 f1 f2 f3) = Product3 (g1 . f1) (g2 . f2) (g3 . f3)
    
instance NFx (Product4 a1 a2 a3 a4) (Product4 b1 b2 b3 b4) (Product4 c1 c2 c3 c4) where
    ncompose (Product4 g1 g2 g3 g4) (Product4 f1 f2 f3 f4) = Product4 (g1 . f1) (g2 . f2) (g3 . f3) (g4 . f4)
    
instance NFx (Product5 a1 a2 a3 a4 a5) (Product5 b1 b2 b3 b4 b5) (Product5 c1 c2 c3 c4 c5) where
    ncompose (Product5 g1 g2 g3 g4 g5) (Product5 f1 f2 f3 f4 f5) = Product5 (g1 . f1) (g2 . f2) (g3 . f3) (g4 . f4) (g5 . f5)

    
