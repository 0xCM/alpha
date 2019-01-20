-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Structures.Module
(
    module X,
    LeftModule(..), 
    RightModule(..),
    Module(..),
    Bimodule(..),
    Basis(..),
    FiniteBasis(..),
        
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Ring as X
import Alpha.Canonical.Structures.StructuredSets as X

-- Note that the category of left modules over a ring R is isomorphic to
-- the category or right modules over the opposite ring R^op

-- | A left module over a ring r
-- See Y2018MTLA,
class (Ring r, AbelianGroup m, LeftAction r m) => LeftModule r m where


-- | A right module over a ring r
class (Ring r, AbelianGroup m, RightAction m r) => RightModule m r where
    
-- | Represents a bimodule where both left and right rings are the same
-- See https://en.wikipedia.org/wiki/Bimodule   
type Module r m = (LeftModule r m, RightModule m r)

-- | Represents a bimodule where left and right rings potentially differ
-- See https://en.wikipedia.org/wiki/Bimodule   
class (LeftModule r m, RightModule m s) => Bimodule r m s where

    
-- | Characterizes a module basis    
class (LeftModule r m, s ~ BasisSet m) => Basis r m s where
    basis::s -> BasisSet m
            
-- | Characterizes a finite module basis 
class (KnownNat n, Finite m, Basis r m s) => FiniteBasis n r m s where


-- | A free module is a module with a basis
-- See https://en.wikipedia.org/wiki/Free_module    
class (LeftModule r m, Basis r m s) => FreeModule r m s where


data ChainComplex = ChainComplex (forall r m. LeftModule r m => [(Integer, m)])

instance Structure 2 LeftModule
instance Structure 2 RightModule
instance Structure 3 Bimodule
instance Structure 3 FreeModule

-- | Captures the invariant that every Abelian group is a module over the
-- ring of integers
instance (AbelianGroup m, LeftAction Integer m) => LeftModule Integer m

-- | Represents a module homomorphism, otherwise known as a linear map
newtype ModuleHom a b  = ModuleHom (a -> b)
    deriving(Generic,Functor)
instance Newtype(ModuleHom a b)

instance Category ModuleHom where
    id = ModuleHom(\x -> x)
    g . f = wrap $ (unwrap g) . (unwrap f)

instance End ModuleHom where
    end x = unwrap x 

-- instance Function ModuleHom where
--     fx f = end f