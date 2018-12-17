module Alpha.Canonical.Algebra.Combinable
(
        

) where
import Alpha.Base
import Alpha.Native

newtype Combiner a b c = Combiner (a -> b -> c)


-- Represents a transformation accepting potentially heterogenous types 
-- a and b and producing a value representing their combination
class Combinable a b where
    type Combined a b
    
    combiner::Combiner a b (Combined a b)
        
    -- Combines two heterogenous values into one
    combine::a -> b -> Combined a b
    combine a b = f a b 
        where (Combiner f)  =  combiner 
    {-# INLINE combine #-}

    -- Infix alias for 'combine'
    (>.<)::a -> b -> Combined a b
    (>.<) = combine        
    {-# INLINE (>.<) #-}
    infixr 0 >.<

    