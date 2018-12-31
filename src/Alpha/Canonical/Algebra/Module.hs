-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Algebra.Module
(
    LeftModule(..), 
    RightModule(..),
    Module(..),
    Bimodule(..),
    Basis(..),
    FiniteBasis(..),
    SpanningSet(..),
    IndependentSet(..),
    BasisSet(..)
        
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Action
import Alpha.Canonical.Algebra.Ring
import Alpha.Canonical.Common.Asci
import Alpha.Canonical.Algebra.ModuleSets

-- Note that the category of left modules over a ring R is isomorphic to
-- the category or right modules over the opposite ring R^op

-- | A left module over a ring r
-- See Y2018MTLA,
class (Ring r, AbelianGroup m, LeftAction r m) => LeftModule r m where

-- | A right module over a ring r
class (Ring r, AbelianGroup m, RightAction m r) => RightModule m r where
    
-- | Represents a bimodule where both left and right rings are the same
-- See https://en.wikipedia.org/wiki/Bimodule   
class (LeftModule r m, RightModule m r) => Module r m where

-- | Represents a bimodule where left and right rings potentially differ
-- See https://en.wikipedia.org/wiki/Bimodule   
class (LeftModule r m, RightModule m s) => Bimodule r m s where
    
-- | Characterizes a module basis    
class (LeftModule r m, s ~ BasisSet r m) => Basis r m s where
    basis::s -> BasisSet r m
            
-- | Characterizes a finite module basis 
class (KnownNat n, Basis r m s) => FiniteBasis n r m s where
    dim::(Integral i) => BasisSet r m -> i
    dim _ = natg @n


-- | A free module is a module with a basis
-- See https://en.wikipedia.org/wiki/Free_module    
class (LeftModule r m, Basis r m s) => FreeModule r m s where


data ChainComplex = ChainComplex (forall r m. LeftModule r m => [(Integer, m)])


-- | Captures the invariant that every Abelian group is a module over the
-- ring of integers
instance (AbelianGroup m, LeftAction Integer m) => LeftModule Integer m

newtype ModuleHom a b  = ModuleHom (a -> b)
    deriving(Generic)
instance Newtype(ModuleHom a b)

instance Functor (ModuleHom a)  where
    fmap f (ModuleHom h) = undefined