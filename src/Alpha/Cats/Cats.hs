{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Cats.Cats where

import Data.Type.Equality
import Data.Type.Coercion
import qualified GHC.Base as B
import GHC.Prim(coerce)
import qualified Control.Category as C
import Alpha.Data.Set
import Prelude(map)



class (Category a, Category b) => Functor f a b where
    omap::forall x y dom cod. (dom ~ Object a x, cod ~ Object b y) => dom -> cod
    amap::forall x y z dom cod. (dom ~ Arrow a x y, cod ~ Arrow b y z) => dom -> cod



class Category c where
    type Object c x
    type Object c x = x
    
    type Arrow c x y
    type Arrow c x y = Object c x -> Object c y
    
    --id::Object c x -> Object c x 
    --id x = x 

    id::Arrow c x x
    
    (.)::Arrow c y z -> Arrow c x y -> Arrow c x z 
    

    
instance Category (->) where    
    (.) = (B..)
    id = B.id

instance Category (Set a) where
    (.) = (B..)
    id = B.id

instance Category ([a]) where
    type Object [a] x = [x]
    (.) = (B..)
    id = B.id


instance Category Coercion where
    type Arrow Coercion x y = Coercion x y
    id = Coercion
    (.) Coercion = coerce
        
