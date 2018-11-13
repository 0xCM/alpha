-----------------------------------------------------------------------------
-- | Defines abstractions over functions
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Composition
(
    Composition(..)
)
where
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product
import Alpha.Data.Sum
import Alpha.Data.Func
    
-- Characterizes function composition
class Composition a b c where
    compose::Hom b c -> Hom a b -> Hom a c
    compose g f = g . f

instance Composition (Product2 b c) (Product2 a b) (Product2 a c)        
