-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Common.Concat
(
    module X,
    Concatenable(..), 
    BiConcatenable(..),
    TypeCast(..),
) where
import Alpha.Base as X
import qualified Data.List as List
import qualified Data.Text as Text

-- | Characterizes a pair whose terms can be related via an append operation
class BiConcatenable a b where
    type BiConcatenated a b

    biconcat::a -> b -> BiConcatenated a b
    biconcat = (>++<)
    {-# INLINE biconcat #-}

    (>++<)::a -> b -> BiConcatenated a b
    (>++<) x y = biconcat x y
    {-# INLINE (>++<) #-}
    infixr 5 >++<

-- | Characterizes a type whose values can be concatenated to yield a value
-- of the same type. This class is essentially equivalent to the 'Semigroup'
-- sans algebraic semantics
class Concatenable a where
    concat::a -> a -> a
    concat = (++)
    {-# INLINE concat #-}

    (++)::a -> a -> a
    (++) x y = concat x y
    {-# INLINE (++) #-}
    infixr 5 ++


-- * Concatenable instances
-------------------------------------------------------------------------------        

instance Concatenable [a] where
    concat = (List.++)            
    
instance Concatenable Text where
    concat = Text.append    

instance Concatenable (Seq a) where
    concat a b = a <> b
    
-- * BiConcatenable instances
-------------------------------------------------------------------------------            
instance BiConcatenable Text Char where
    type BiConcatenated Text Char = Text
    biconcat t c  =  Text.append t $ Text.pack [c]
    
instance BiConcatenable Char Text where    
    type BiConcatenated Char Text = Text
    biconcat c t  = Text.append (Text.pack [c]) t
    
instance BiConcatenable Char Char where    
    type BiConcatenated Char Char = Text
    biconcat c1 c2  = Text.pack ([c1] List.++ [c2])            

class TypeCast a b | a -> b, b -> a where 
    cast::a -> b

class TypeCast' t a b | t a -> b, t b -> a where 
    cast'::t -> a -> b

class TypeCast'' t a b | t a -> b, t b -> a where 
    cast''::t -> a -> b

instance TypeCast' () a b => TypeCast a b where 
    cast x = cast' () x

instance TypeCast'' t a b => TypeCast' t a b where 
    cast' = cast''

instance TypeCast'' () a a where 
    cast'' _ x  = x    

instance TypeCast Int Integer where
    cast = fromIntegral
