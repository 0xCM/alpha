{-# LANGUAGE DataKinds #-}
module Alpha.Numerics.Modular
(
    Zn, zN, modulus, residues,residue,
    based, base2, base10, base16,

    Base, BasedInt

)
where
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers
import qualified Data.Set as Set

data Base (n::Nat) = Base

data BasedInt (n::Nat) i = BasedInt !i

based::forall (n::Nat) i. (Integral i) => i -> BasedInt n i
based i = BasedInt i

base2 = based @2

base10 = based @10

base16 = based @16

-- | Represents the ring of integers mod n
data Zn (n::Nat) = Zn Integer
    deriving (Eq, Ord)

data Residue (n::Nat) = Residue (Zn n) Integer
    deriving (Eq, Ord)

-- Constructs a representation for the ring of integers mod n
zN::forall n. KnownNat n => Zn n
zN = Zn (natVal (Proxy @n))

modulus::forall n.KnownNat n => Zn n -> Integer
modulus (Zn n) = (fromInteger n) `mod` (natVal (Proxy @n))

residue::(KnownNat n) => Integer -> Residue n 
residue m = Residue (zN) m

-- See https://github.com/TikhonJelvis/modular-arithmetic/blob/master/src/Data/Modular.hs

-- | The canonical least residues modulo n
-- See https://en.wikipedia.org/wiki/Modular_arithmetic for terminology
residues::KnownNat n => Zn n -> [Residue n]    
residues (Zn n) = fmap residue [0..(n-1)] 

instance KnownNat n => Show (Zn n) where
    show _ = "Z/" ++ nn where
        nn = show (natVal (Proxy @n))

instance  KnownNat n => Membership (Zn n) where    
    type Member (Zn n) = Residue n
    members s = residues s |> Set.fromList

instance KnownNat n => Show (Residue n) where
    show (Residue zN m) = (show m) ++ " " ++ (show zN)

instance KnownNat n => Additive (Residue n) where
    add (Residue zN a) (Residue _ b) = residue( (a + b) `mod` (modulus zN) )

instance KnownNat n => Multiplicative (Residue n) where
    mul (Residue zN a) (Residue _ b) = residue( (a * b) `mod` (modulus zN) )    