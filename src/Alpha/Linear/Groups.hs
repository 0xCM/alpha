-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Groups
(
    module X,
    LinearGroup(..),
    GL(..),
)
where
import Alpha.Canonical
import Alpha.Linear.Matrix as X

class KnownNat n => LinearGroup n g where


newtype GL n a = GL (SquareMatrix n a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable,Indexable, Discrete)
    deriving (Show,Formattable) via (SquareMatrix n a)

type instance Individual (GL n a) = a

instance SymbolicName "GL(n)" (GL n a)
instance KnownNat n => LinearGroup n (GL n a)

instance Newtype (GL n a)
deriving instance KnownNat n => Dimensional (GL n a)
deriving instance KnownNat n => Transposable (GL n a)
deriving instance (KnownNat n,  Nullary a) => Nullary (GL n a)
deriving instance (KnownNat n,  Unital a, Nullary a) => Unital (GL n a)
deriving instance (KnownNat n,  Additive a) => Additive (GL n a)
deriving instance (KnownNat n,  Subtractive a) => Subtractive (GL n a)
deriving instance (KnownNat n,  Negatable a) => Negatable (GL n a)
deriving instance (KnownNat n,  AbelianGroup a) => AbelianGroup (GL n a)
deriving instance (KnownNat n, LeftAction k a) => LeftAction k (GL n a)

instance (KnownNat n) => Invertible (GL n a) where
    invert = undefined

