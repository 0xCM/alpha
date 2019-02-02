module Alpha.Data.BitVector
(
    BitVector,
    bitvector,
) where
import Alpha.Canonical
import Alpha.Data.Bit

-- | Represents a finite sequence of bits
newtype BitVector n = BitVector Natural
    deriving (Eq, Ord, Generic, Data, Typeable, Functor)

type instance Individual (BitVector n) = Bit

bitvector::(KnownNat n) => [Bit] -> BitVector n
bitvector src = natural <$> src |> reduce 0 (+) |> BitVector

-------------------------------------------------------------------------------
-- **BitVector membership
-------------------------------------------------------------------------------    
instance (KnownNat n) => Indexable (BitVector n) where
    idx (BitVector bv) i = ifelse (bv .&. (natural i) == 1) on off

instance (KnownNat n) => SafeIx (BitVector n) Int where
    (BitVector bv) !? i = ifelse (i <= (nat @n - 1)) (Just (bv !! i)) Nothing        

instance (KnownNat n) => Complementable (BitVector n) where
    complement (BitVector bv) = complement' bv |> BitVector