{-# LANGUAGE DataKinds #-}
module Alpha.Numerics.Modular
(
    Zn, zN, modulus, leastResidues,residue,
    based, base2, base10, base16,

    Base, BasedInt

)
where
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers

data Base (n::Nat) = Base

data BasedInt (n::Nat) i = BasedInt !i

based::forall (n::Nat) i. (Integral i) => i -> BasedInt n i
based i = BasedInt i

base2 = based @2

base10 = based @10

base16 = based @16

-- |Represents the ring of integers mod n
data Zn (n::Nat) = Zn Integer

data Residue (n::Nat) m = Residue (Zn n)  m 

-- Constructs a representation for the ring of integers mod n
zN::forall n. KnownNat n => Zn n
zN = Zn (natVal (Proxy @n))

modulus::(KnownNat n, Integral m) => Zn n -> m
modulus (Zn n) = (fromInteger n)

residue::(KnownNat n, Integral m) => m -> Residue n m
residue m = Residue (zN) m

-- See https://github.com/TikhonJelvis/modular-arithmetic/blob/master/src/Data/Modular.hs

-- | The canonical residues modulo n
-- See https://en.wikipedia.org/wiki/Modular_arithmetic for terminology
leastResidues::KnownNat n => Zn n -> [Integer]    
leastResidues (Zn n) = [0..(n-1)] 

type ModN n m = (KnownNat n, Integral m, Additive m)

instance KnownNat n => Show (Zn n) where
    show _ = "Z/" ++ nn where
        nn = show (natVal (Proxy @n))

instance KnownNat n => FiniteMembership (Zn n) Integer where    
    members = leastResidues

instance (Show m, ModN n m) => Show(Residue n m) where
    show (Residue zN m) = (show m) ++ " " ++ (show zN)

instance (ModN n m) => Additive (Residue n m) where
    add (Residue zN a) (Residue _ b) = residue( (a + b) `mod` (modulus zN) )