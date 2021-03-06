-----------------------------------------------------------------------------
-- | A type-level bitvector, derivative from Galois bv-sized and 
-- parameterized-utils libraries; see LICENSE
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Data.BitVector
(
  --BitVector(..),(<:>),ToBitVector(..),bitvector
)
where
import Data.Bits
import Data.Hashable
import Data.Ix
import Data.Functor
import Data.Int
import Data.Foldable
import Data.Ord(compare)
import Data.Kind(Type)
import Data.Proxy as Proxy
import Data.Type.Equality as Equality
import GHC.Read
import GHC.TypeLits
import GHC.Num
import GHC.Enum
import GHC.Real
import GHC.Natural
import Unsafe.Coerce
import Text.Printf
import Numeric

import Alpha.Canonical hiding(index, (+), (-), (*), abs, mod,rem,complement)
import Alpha.Base
import GHC.Real(mod)
import Prelude(($!))

import qualified GHC.Natural as TN
import qualified GHC.TypeLits as TL

maxInt::Integer
maxInt = integer (maxBound :: Int)

knownNat :: forall n . KnownNat n => NatRepr n
knownNat = NatRepr (natg @n)
      
-- | Return the value of the nat representation.
repVal :: NatRepr n -> Int
repVal (NatRepr i) | i < maxInt = fromInteger i
                    | otherwise = error "Width is too large."

addNat :: NatRepr m -> NatRepr n -> NatRepr (m+n)
addNat (NatRepr m) (NatRepr n) = NatRepr (m+n)

data BitVector (w :: Nat) :: Type where
  BV :: NatRepr w -> Integer -> BitVector w

class (KnownNat w) => ToBitVector a w where
  bv::a -> BitVector w
  
  
-- | Construct a bit vector with a particular width, where the width is inferrable
-- from the type context. The 'Integer' input (an unbounded data type, hence with an
-- infinite-width bit representation), whether positive or negative, is silently
-- truncated to fit into the number of bits demanded by the return type.
--
-- >>> bitvector 0xA :: BitVector 4
-- 0xa
-- >>> bitvector 0xA :: BitVector 2
-- 0x2
  
bitvector :: KnownNat w => Integer -> BitVector w
bitvector x = BV wRepr (truncBits width (fromIntegral x))
  where wRepr = knownNat
        width = natValue wRepr

-- The zero bitvector
bv0 :: BitVector 0
bv0 = bitvector 0
        
-- | Mask for a specified number of lower bits.
lowMask :: (Integral a, Bits b) => a -> b
lowMask numBits = complement (complement zeroBits `shiftL` fromIntegral numBits)

-- | Truncate to a specified number of lower bits.
truncBits :: (Integral a, Bits b) => a -> b -> b
truncBits width b = b .&. lowMask width
  
----------------------------------------
-- Bits

----------------------------------------
-- BitVector w operations (fixed width)

-- | Bitwise and.
bvAnd :: BitVector w -> BitVector w -> BitVector w
bvAnd (BV wRepr x) (BV _ y) = BV wRepr (x .&. y)

-- | Bitwise or.
bvOr :: BitVector w -> BitVector w -> BitVector w
bvOr (BV wRepr x) (BV _ y) = BV wRepr (x .|. y)

-- | Bitwise xor.
bvXor :: BitVector w -> BitVector w -> BitVector w
bvXor (BV wRepr x) (BV _ y) = BV wRepr (x `xor` y)

-- | Bitwise complement (flip every bit).
bvComplement :: BitVector w -> BitVector w
bvComplement (BV wRepr x) = BV wRepr (truncBits width (complement x))
  where width = natValue wRepr

-- | Bitwise shift. Uses an arithmetic right shift.
bvShift :: BitVector w -> Int -> BitVector w
bvShift bv@(BV wRepr _) shf = BV wRepr (truncBits width (x `shift` shf))
  where width = natValue wRepr
        x     = bvIntegerS bv -- arithmetic right shift when negative

toPos :: Int -> Int
toPos x | x < 0 = 0
toPos x = x

-- | Left shift.
bvShiftL :: BitVector w -> Int -> BitVector w
bvShiftL bv shf = bvShift bv (toPos shf)

-- | Right arithmetic shift.
bvShiftRA :: BitVector w -> Int -> BitVector w
bvShiftRA bv shf = bvShift bv (- (toPos shf))

-- | Right logical shift.
bvShiftRL :: BitVector w -> Int -> BitVector w
bvShiftRL bv@(BV wRepr _) shf = BV wRepr (truncBits width (x `shift` (- toPos shf)))
  where width = natValue wRepr
        x     = bvIntegerU bv

-- | Bitwise rotate.
bvRotate :: BitVector w -> Int -> BitVector w
bvRotate bv rot' = leftChunk `bvOr` rightChunk
  where rot = rot' `mod` bvWidth bv
        leftChunk = bvShift bv rot
        rightChunk = bvShift bv (rot - bvWidth bv)

-- | Get the width of a 'BitVector'.
bvWidth :: BitVector w -> Int
bvWidth (BV wRepr _) = fromIntegral (natValue wRepr)

-- | Test if a particular bit is set.
bvTestBit :: BitVector w -> Int -> Bool
bvTestBit (BV _ x) b = testBit x b

-- | Get the number of 1 bits in a 'BitVector'.
bvPopCount :: BitVector w -> Int
bvPopCount (BV _ x) = popCount x


----------------------------------------
-- BitVector -> Integer functions

-- | Unsigned interpretation of a bit vector as a (positive) Integer.
bvIntegerU :: BitVector w -> Integer
bvIntegerU (BV _ x) = x

-- | Signed interpretation of a bit vector as an Integer.
bvIntegerS :: BitVector w -> Integer
bvIntegerS bv = if bvTestBit bv (width - 1)
                then bvIntegerU bv - (1 `shiftL` width)
                else bvIntegerU bv
  where width = bvWidth bv

----------------------------------------
-- BitVector w arithmetic operations (fixed width)

-- | Bitwise add.
bvAdd :: BitVector w -> BitVector w -> BitVector w
bvAdd (BV wRepr x) (BV _ y) = BV wRepr (truncBits width (x + y))
  where width = natValue wRepr

-- | Bitwise multiply.
bvMul :: BitVector w -> BitVector w -> BitVector w
bvMul (BV wRepr x) (BV _ y) = BV wRepr (truncBits width (x * y))
  where width = natValue wRepr

-- | Bitwise division (unsigned). Rounds to zero.
bvQuotU :: BitVector w -> BitVector w -> BitVector w
bvQuotU (BV wRepr x) (BV _ y) = BV wRepr (x `quot'` y)

-- | Bitwise division (signed). Rounds to zero (not negative infinity).
bvQuotS :: BitVector w -> BitVector w -> BitVector w
bvQuotS bv1@(BV wRepr _) bv2 = BV wRepr (truncBits width (x `quot'` y))
  where x = bvIntegerS bv1
        y = bvIntegerS bv2
        width = natValue wRepr

-- | Bitwise remainder after division (unsigned), when rounded to zero.
bvRemU :: BitVector w -> BitVector w -> BitVector w
bvRemU (BV wRepr x) (BV _ y) = BV wRepr (x `rem'` y)

-- | Bitwise remainder after  division (signed), when rounded to zero (not negative
-- infinity).
bvRemS :: BitVector w -> BitVector w -> BitVector w
bvRemS bv1@(BV wRepr _) bv2 = BV wRepr (truncBits width (x `rem'` y))
  where x = bvIntegerS bv1
        y = bvIntegerS bv2
        width = natValue wRepr

-- | Bitwise absolute value.
bvAbs :: BitVector w -> BitVector w
bvAbs bv@(BV wRepr _) = BV wRepr abs_x
  where width = natValue wRepr
        x     = bvIntegerS bv
        abs_x = truncBits width (abs x) -- this is necessary

-- | Bitwise negation.
bvNegate :: BitVector w -> BitVector w
bvNegate (BV wRepr x) = BV wRepr (truncBits width (-x))
  where width = fromIntegral (natValue wRepr) :: Integer

-- | Get the sign bit as a 'BitVector'.
bvSignum :: BitVector w -> BitVector w
bvSignum bv@(BV wRepr _) = bvShift bv (1 - width) `bvAnd` BV wRepr 0x1
  where width = fromIntegral (natValue wRepr)

-- | Signed less than.
bvLTS :: BitVector w -> BitVector w -> Bool
bvLTS bv1 bv2 = bvIntegerS bv1 < bvIntegerS bv2

-- | Unsigned less than.
bvLTU :: BitVector w -> BitVector w -> Bool
bvLTU bv1 bv2 = bvIntegerU bv1 < bvIntegerU bv2

----------------------------------------
-- Width-changing operations

-- | Concatenate two bit vectors.
--
-- >>> (0xAA :: BitVector 8) `bvConcat` (0xBCDEF0 :: BitVector 24)
-- 0xaabcdef0
-- >>> :type it
-- it :: BitVector 32
--
-- Note that the first argument gets placed in the higher-order bits. The above
-- example should be illustrative enough.
bvConcat :: BitVector v -> BitVector w -> BitVector (v + w)
bvConcat (BV hiWRepr hi) (BV loWRepr lo) =
  BV (hiWRepr `addNat` loWRepr) ((hi `shiftL` loWidth) .|. lo)
  where loWidth = fromIntegral (natValue loWRepr)

-- | Infix 'bvConcat'.
(<:>) :: BitVector v -> BitVector w -> BitVector (v+w)
(<:>) = bvConcat

bvConcatSome :: Some BitVector -> Some BitVector -> Some BitVector
bvConcatSome (Some bv1) (Some bv2) = Some (bv2 <:> bv1)

-- | Concatenate a list of 'BitVector's into a 'BitVector' of arbitrary width. The ordering is little endian:
--
-- >>> bvConcatMany [0xAA :: BitVector 8, 0xBB] :: BitVector 16
-- 0xbbaa
-- >>> bvConcatMany [0xAA :: BitVector 8, 0xBB, 0xCC] :: BitVector 16
-- 0xbbaa
--
-- If the sum of the widths of the input 'BitVector's exceeds the output width, we
-- ignore the tail end of the list.
bvConcatMany :: KnownNat w' => [BitVector w] -> BitVector w'
bvConcatMany = bvConcatManyWithRepr knownNat

-- | 'bvConcatMany' with an explicit 'NatRepr'.
bvConcatManyWithRepr :: NatRepr w' -> [BitVector w] -> BitVector w'
bvConcatManyWithRepr wRepr bvs =
  viewSome (bvZextWithRepr wRepr) $ foldl bvConcatSome (Some bv0) (Some <$> bvs)

infixl 6 <:>

-- | Slice out a smaller bit vector from a larger one. The lowest significant bit is
-- given explicitly as an argument of type 'Int', and the length of the slice is
-- inferred from a type-level context.
--
-- >>> bvExtract 12 (0xAABCDEF0 :: BitVector 32) :: BitVector 8
-- 0xcd
--
-- Note that 'bvExtract' does not do any bounds checking whatsoever; if you try and
-- extract bits that aren't present in the input, you will get 0's.
bvExtract :: forall w w' . (KnownNat w')
          => Int
          -> BitVector w
          -> BitVector w'
bvExtract pos bv = bitvector xShf
  where (BV _ xShf) = bvShift bv (- pos)

-- | Unconstrained variant of 'bvExtract' with an explicit 'NatRepr' argument.
bvExtractWithRepr :: NatRepr w'
                  -> Int
                  -> BitVector w
                  -> BitVector w'
bvExtractWithRepr repr pos bv = BV repr (truncBits width xShf)
  where (BV _ xShf) = bvShift bv (- pos)
        width = natValue repr

-- | Zero-extend a vector to one of greater length. If given an input of greater
-- length than the output type, this performs a truncation.
bvZext :: (KnownNat w1, KnownNat w2) => BitVector w1 -> BitVector w2
bvZext (BV _ x) = bitvector x

-- | Unconstrained variant of 'bvZext' with an explicit 'NatRepr' argument.
bvZextWithRepr :: NatRepr w' -> BitVector w -> BitVector w'
bvZextWithRepr repr (BV _ x) = BV repr (truncBits width x)
  where width = natValue repr

-- | Sign-extend a vector to one of greater length. If given an input of greater
-- length than the output type, this performs a truncation.
bvSext :: (KnownNat w1, KnownNat w2) => BitVector w1 -> BitVector w2
bvSext bv = bitvector (bvIntegerS bv)

-- | Unconstrained variant of 'bvSext' with an explicit 'NatRepr' argument.
bvSextWithRepr :: NatRepr w'
               -> BitVector w
               -> BitVector w'
bvSextWithRepr repr bv = BV repr (truncBits width (bvIntegerS bv))
  where width = natValue repr



instance Formattable (BitVector w) where
  format (BV w x ) = bitTextW (repVal w) x

instance KnownNat w => Read (BitVector w) where
  readsPrec s =
    (fmap . fmap) (\(a,s') -> (bitvector a, s')) (readsPrec s :: ReadS Integer)

instance Eq (BitVector w) where
  (BV _ x) == (BV _ y) = x == y

instance Ord (BitVector w) where
  (BV _ x) `compare` (BV _ y) = x `compare` y

instance TestEquality BitVector where
  testEquality (BV wRepr x) (BV wRepr' y) =
    if natValue wRepr == natValue wRepr' && x == y
    then Just (unsafeCoerce (Refl :: a :~: a))
    else Nothing

instance KnownNat w => Bits (BitVector w) where
  (.&.)        = bvAnd
  (.|.)        = bvOr
  xor          = bvXor
  complement   = bvComplement
  shift        = bvShift
  rotate       = bvRotate
  bitSize      = bvWidth
  bitSizeMaybe = Just . bvWidth
  isSigned     = const False
  testBit      = bvTestBit
  bit x          = bitvector (fromIntegral x)
  popCount     = bvPopCount

instance KnownNat w => FiniteBits (BitVector w) where
  finiteBitSize = bvWidth

instance (KnownNat w) => Num (BitVector w) where
  (+)         = bvAdd
  (*)         = bvMul
  abs         = bvAbs
  signum      = bvSignum
  fromInteger = bitvector
  negate      = bvNegate

instance KnownNat w => Enum (BitVector w) where
  toEnum   = bitvector . fromIntegral
  fromEnum = fromIntegral . bvIntegerU

instance KnownNat w => Ix (BitVector w) where
  range (lo, hi) = bitvector <$> [bvIntegerU lo .. bvIntegerU hi]
  index (lo, hi) bv = index (bvIntegerU lo, bvIntegerU hi) (bvIntegerU bv)
  inRange (lo, hi) bv = inRange (bvIntegerU lo, bvIntegerU hi) (bvIntegerU bv)

instance KnownNat w => Bounded (BitVector w) where
  minBound = bitvector 0
  maxBound = bitvector (-1)

instance (KnownNat w)  => ToBitVector Integer w where
  bv = bitvector

instance ToBitVector Word8 8 where
  bv x = bitvector (fromIntegral x)

instance ToBitVector Word16 16 where
  bv x = bitvector (fromIntegral x)

instance ToBitVector Word32 32 where
  bv x = bitvector (fromIntegral x)

instance ToBitVector Word64 64 where
  bv x = bitvector (fromIntegral x)

instance (KnownNat w1, KnownNat w2) => BiConcatenable (BitVector w1) (BitVector w2)  where
  type BiConcatenated (BitVector w1) (BitVector w2) = BitVector (w1 + w2)
  biconcat = bvConcat

instance KnownNat w => Length (BitVector w) where
  length  = fromIntegral . finiteBitSize  

newtype NatRepr (n::Nat) = NatRepr {
    natValue :: Integer
    } deriving (Hashable)
 
 
instance Eq (NatRepr m) where
    _ == _ = True

instance TestEquality NatRepr where
    testEquality (NatRepr m) (NatRepr n)
        | m == n = Just (unsafeCoerce Refl)
        | otherwise = Nothing

instance (KnownNat n) => Reifiable n Integer where
    reify = TL.natVal (Proxy::Proxy n)
        

data Some (f:: k -> *) = forall x . Some (f x)

viewSome :: (forall tp . f tp -> r) -> Some f -> r
viewSome f (Some x) = f x

mapSome :: (forall tp . f tp -> g tp) -> Some f -> Some g
mapSome f (Some x) = Some $! f x

{-# INLINE traverseSome #-}
-- | Modify the inner value.
traverseSome :: Functor m
                => (forall tp . f tp -> m (g tp))
                -> Some f
                -> m (Some g)
traverseSome f (Some x) = Some `fmap` f x


-- | Modify the inner value.
traverseSome_ :: Functor m => (forall tp . f tp -> m ()) -> Some f -> m ()
traverseSome_ f (Some x) = (\_ -> ()) `fmap` f x
{-# INLINE traverseSome_ #-}    
      