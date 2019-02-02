-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Linear.Tensor
(

)
where
import Alpha.Canonical hiding(Product)

-- | Represents an element of a tensor product space
data a <++> b = Tensor a b  
infixr 5 <++>

-- | Form an element of the tensor product space
(<++>)::a -> b -> a <++> b
(<++>) a b = Tensor a b

-- | Represents a tensor product space which, by means of
-- the universal property of tensor products, is guaranteed
-- to exist whenever a and b are of a suitable category,
-- e.g., vector spaces
type family a <+> b where
    a <+> b = a <++> b

-- | Represents an element of a product space
data a <**> b = Product a b
infixr 6 <**>

-- | Form an element of the product space
(<**>)::a -> b -> a <**> b
(<**>) a b = Product a b


type family a <*> b where
    a <*> b = a <**> b