{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Structures.TensorProduct
(
    type (<+>), (<+>)    
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Module


data a <+> b = Tensored a b  

infixr 5 <+>

(<+>)::a -> b -> a <+> b
(<+>) a b = Tensored a b