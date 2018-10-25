-----------------------------------------------------------------------------
-- | Support for type-level natural numbers, derivative from Galois bv-sized and 
-- parameterized-utils libraries; see LICENSE
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Alpha.TypeLevel.Nats where
import GHC.TypeLits
import Data.Hashable
import Data.Int
import Data.Word
import Data.Ord
import Data.Maybe
import Data.Either
import Data.Type.Equality as Equality
import Data.Proxy as Proxy
import Unsafe.Coerce
import Data.Kind(Type)
import Data.Eq(Eq(..))
import Data.Bits ((.&.))
import Data.Int
import GHC.Num
import qualified GHC.Natural as TN
import qualified GHC.TypeLits as TL
import Prelude

import Alpha.TypeLevel.Some
import Alpha.TypeLevel.Common
import Alpha.TypeLevel.Proxy
import Alpha.Canonical(Reifiable(..))

instance (KnownNat n) => Reifiable (n::Nat) Integer where
    reify = TL.natVal (proxy @n)

newtype NatRepr (n::Nat) = NatRepr {
   natValue :: Integer
  } deriving (Hashable)

data IsZeroNat n where
    ZeroNat    :: IsZeroNat 0
    NonZeroNat :: IsZeroNat (n+1)


data NatComparison m n where
    -- First number is less than second.
    NatLT :: x+1 <= x+(y+1) => !(NatRepr y) -> NatComparison x (x+(y+1))
    NatEQ :: NatComparison x x
    -- First number is greater than second.
    NatGT :: x+1 <= x+(y+1) => !(NatRepr y) -> NatComparison (x+(y+1)) x

-- | @LeqProof m n@ is a type whose values are only inhabited when @m@
-- is less than or equal to @n@.
data LeqProof m n where
    LeqProof :: (m <= n) => LeqProof m n

-- As for NatComparison above, but works with LeqProof
data NatCases m n where
    -- First number is less than second.
    NatCaseLT :: LeqProof (m+1) n -> NatCases m n
    NatCaseEQ :: NatCases m m
    -- First number is greater than second.
    NatCaseGT :: LeqProof (n+1) m -> NatCases m n
    
maxInt :: Integer
maxInt = toInteger (maxBound :: Int)

knownNat :: forall n . KnownNat n => NatRepr n
knownNat = NatRepr (natVal (Proxy :: Proxy n))

compareNat :: NatRepr m -> NatRepr n -> NatComparison m n
compareNat m n =
  case compare (natValue m) (natValue n) of
    LT -> unsafeCoerce (NatLT @0 @0) (NatRepr (natValue n - natValue m - 1))
    EQ -> unsafeCoerce  NatEQ
    GT -> unsafeCoerce (NatGT @0 @0) (NatRepr (natValue m - natValue n - 1))

      
isZeroNat :: NatRepr n -> IsZeroNat n
isZeroNat (NatRepr 0) = unsafeCoerce ZeroNat
isZeroNat (NatRepr _) = unsafeCoerce NonZeroNat

-- | Return the value of the nat representation.
repVal :: NatRepr n -> Int
repVal (NatRepr i) | i < maxInt = fromInteger i
                     | otherwise = error "Width is too large."

-- | Decrement a @NatRepr@
decNat :: (1 <= n) => NatRepr n -> NatRepr (n-1)
decNat (NatRepr i) = NatRepr (i-1)

-- | Get the antecedent
prior:: NatRepr (n+1) -> NatRepr n
prior (NatRepr i) = NatRepr (i-1)

-- | Get the successor
nextNat:: NatRepr (n) -> NatRepr(n + 1)
nextNat (NatRepr i) = NatRepr (i + 1)

-- | Increment a @NatRepr@
incNat :: NatRepr n -> NatRepr (n+1)
incNat (NatRepr x) = NatRepr (x+1)

halfNat :: NatRepr (n+n) -> NatRepr n
halfNat (NatRepr x) = NatRepr (x `div` 2)

addNat :: NatRepr m -> NatRepr n -> NatRepr (m+n)
addNat (NatRepr m) (NatRepr n) = NatRepr (m+n)

subNat :: (n <= m) => NatRepr m -> NatRepr n -> NatRepr (m-n)
subNat (NatRepr m) (NatRepr n) = NatRepr (m-n)

------------------------------------------------------------------------
-- Operations for using NatRepr as a bitwidth.

-- | Return minimum unsigned value for bitvector with given width (always 0).
minUnsigned :: NatRepr w -> Integer
minUnsigned _ = 0

-- | Return maximum unsigned value for bitvector with given width.
maxUnsigned :: NatRepr w -> Integer
maxUnsigned w = 2^(natValue w) - 1

-- | Return minimum value for bitvector in 2s complement with given width.
minSigned :: (1 <= w) => NatRepr w -> Integer
minSigned w = negate (2^(natValue w - 1))

-- | Return maximum value for bitvector in 2s complement with given width.
maxSigned :: (1 <= w) => NatRepr w -> Integer
maxSigned w = 2^(natValue w - 1) - 1

-- | @toUnsigned w i@ maps @i@ to a @i `mod` 2^w@.
toUnsigned :: NatRepr w -> Integer -> Integer
toUnsigned w i = maxUnsigned w .&. i

-- | @toSigned w i@ interprets the least-significant @w@ bits in @i@ as a
-- signed number in two's complement notation and returns that value.
toSigned :: (1 <= w) => NatRepr w -> Integer -> Integer
toSigned w i0
    | i > maxSigned w = i - 2^(natValue w)
    | otherwise       = i
  where i = i0 .&. maxUnsigned w

-- | @unsignedClamp w i@ rounds @i@ to the nearest value between
-- @0@ and @2^w-1@ (inclusive).
unsignedClamp :: NatRepr w -> Integer -> Integer
unsignedClamp w i
  | i < minUnsigned w = minUnsigned w
  | i > maxUnsigned w = maxUnsigned w
  | otherwise         = i

-- | @signedClamp w i@ rounds @i@ to the nearest value between
-- @-2^(w-1)@ and @2^(w-1)-1@ (inclusive).
signedClamp :: (1 <= w) => NatRepr w -> Integer -> Integer
signedClamp w i
  | i < minSigned w = minSigned w
  | i > maxSigned w = maxSigned w
  | otherwise       = i

------------------------------------------------------------------------
-- Some NatRepr

someNat :: Integer -> Maybe (Some NatRepr)
someNat n | 0 <= n && n <= toInteger maxInt = Just (Some (NatRepr (fromInteger n)))
          | otherwise = Nothing

-- | Return the maximum of two nat representations.
maxNat :: NatRepr m -> NatRepr n -> Some NatRepr
maxNat x y
  | natValue x >= natValue y = Some x
  | otherwise = Some y

------------------------------------------------------------------------
-- Arithmetic

-- | Produce evidence that + is commutative.
plusComm :: forall f m g n . f m -> g n -> m+n :~: n+m
plusComm _ _ = unsafeCoerce (Refl :: m+n :~: m+n)

-- | Produce evidence that * is commutative.
-- mulComm :: forall f m g n. f m -> g n -> (m * n) :~: (n * m)
-- mulComm _ _ = unsafeCoerce Refl

-- mul2Plus :: forall f n. f n -> (n + n) :~: (2 * n)
-- mul2Plus n = case addMulDistribRight (Proxy @1) (Proxy @1) n of
--                Refl -> Refl

-- | Cancel an add followed b a subtract
plusMinusCancel :: forall f m g n . f m -> g n -> (m + n) - n :~: m
plusMinusCancel _ _ = unsafeCoerce (Refl :: m :~: m)

minusPlusCancel :: forall f m g n . (n <= m) => f m -> g n -> (m - n) + n :~: m
minusPlusCancel _ _ = unsafeCoerce (Refl :: m :~: m)


------------------------------------------------------------------------
-- LeqProof

testStrictLeq :: forall m n
                . (m <= n)
            => NatRepr m
            -> NatRepr n
            -> Either (LeqProof (m+1) n) (m :~: n)
testStrictLeq (NatRepr m) (NatRepr n)
    | m < n = Left (unsafeCoerce (LeqProof :: LeqProof 0 0))
    | otherwise = Right (unsafeCoerce (Refl :: m :~: m))
{-# NOINLINE testStrictLeq #-}
    
  
testNatCases ::  forall m n
            . NatRepr m
            -> NatRepr n
            -> NatCases m n
testNatCases m n =
    case compare (natValue m) (natValue n) of
        LT -> NatCaseLT (unsafeCoerce (LeqProof :: LeqProof 0 0))
        EQ -> unsafeCoerce $ (NatCaseEQ :: NatCases m m)
        GT -> NatCaseGT (unsafeCoerce (LeqProof :: LeqProof 0 0))
{-# NOINLINE testNatCases #-}
  

-- | @x `testLeq` y@ checks whether @x@ is less than or equal to @y@.
testLeq :: forall m n . NatRepr m -> NatRepr n -> Maybe (LeqProof m n)
testLeq (NatRepr m) (NatRepr n)
   | m <= n    = Just (unsafeCoerce (LeqProof :: LeqProof 0 0))
   | otherwise = Nothing
{-# NOINLINE testLeq #-}

-- | Apply reflexivity to LeqProof
leqRefl :: forall f n . f n -> LeqProof n n
leqRefl _ = LeqProof

-- | Apply transitivity to LeqProof
leqTrans :: LeqProof m n -> LeqProof n p -> LeqProof m p
leqTrans LeqProof LeqProof = unsafeCoerce (LeqProof :: LeqProof 0 0)
{-# NOINLINE leqTrans #-}

-- | Add both sides of two inequalities
leqAdd2 :: LeqProof x_l x_h -> LeqProof y_l y_h -> LeqProof (x_l + y_l) (x_h + y_h)
leqAdd2 x y = seq x $ seq y $ unsafeCoerce (LeqProof :: LeqProof 0 0)
{-# NOINLINE leqAdd2 #-}

-- | Subtract sides of two inequalities.
leqSub2 :: LeqProof x_l x_h
        -> LeqProof y_l y_h
        -> LeqProof (x_l-y_h) (x_h-y_l)
leqSub2 LeqProof LeqProof = unsafeCoerce (LeqProof :: LeqProof 0 0)
{-# NOINLINE leqSub2 #-}

------------------------------------------------------------------------
-- LeqProof combinators

-- | Create a leqProof using two proxies
leqProof :: (m <= n) => f m -> g n -> LeqProof m n
leqProof _ _ = LeqProof

withLeqProof :: LeqProof m n -> ((m <= n) => a) -> a
withLeqProof p a =
  case p of
    LeqProof -> a

-- | Test whether natural number is positive.
isPosNat :: NatRepr n -> Maybe (LeqProof 1 n)
isPosNat = testLeq (knownNat :: NatRepr 1)

leqAdd :: forall f m n p . LeqProof m n -> f p -> LeqProof m (n+p)
leqAdd x _ = leqAdd2 x (LeqProof :: LeqProof 0 p)

leqAddPos :: (1 <= m, 1 <= n) => p m -> q n -> LeqProof 1 (m + n)
leqAddPos m n = leqAdd (leqProof (Proxy :: Proxy 1) m) n

-- | Produce proof that subtracting a value from the smaller element is smaller.
leqSub :: forall m n p . LeqProof m n -> LeqProof p m -> LeqProof (m-p) n
leqSub x _ = leqSub2 x (LeqProof :: LeqProof 0 p)

addIsLeq :: f n -> g m -> LeqProof n (n + m)
addIsLeq n m = leqAdd (leqRefl n) m

addPrefixIsLeq :: f m -> g n -> LeqProof n (m + n)
addPrefixIsLeq m n =
  case plusComm n m of
    Refl -> addIsLeq n m

dblPosIsPos :: forall n . LeqProof 1 n -> LeqProof 1 (n+n)
dblPosIsPos x = leqAdd x Proxy

addIsLeqLeft1 :: forall n n' m . LeqProof (n + n') m -> LeqProof n m
addIsLeqLeft1 p =
    case plusMinusCancel n n' of
      Refl -> leqSub p le
  where n :: Proxy n
        n = Proxy
        n' :: Proxy n'
        n' = Proxy
        le :: LeqProof n' (n + n')
        le = addPrefixIsLeq n n'

{-# INLINE withAddPrefixLeq #-}
withAddPrefixLeq :: NatRepr n -> NatRepr m -> ((m <= n + m) => a) -> a
withAddPrefixLeq n m = withLeqProof (addPrefixIsLeq n m)

withAddLeq :: forall n m a. NatRepr n -> NatRepr m -> ((n <= n + m) => NatRepr (n + m) -> a) -> a
withAddLeq n m f = withLeqProof (addIsLeq n m) (f (addNat n m))

natForEach' :: forall l h a
            . NatRepr l
            -> NatRepr h
            -> (forall n. LeqProof l n -> LeqProof n h -> NatRepr n -> a)
            -> [a]
natForEach' l h f
  | Just LeqProof  <- testLeq l h =
    let f' :: forall n. LeqProof (l + 1) n -> LeqProof n h -> NatRepr n -> a
        f' = \lp hp -> f (addIsLeqLeft1 lp) hp
     in f LeqProof LeqProof l : natForEach' (incNat l) h f'
  | otherwise             = []

-- | Apply a function to each element in a range; return the list of values
-- obtained.
natForEach :: forall l h a
            . NatRepr l
           -> NatRepr h
           -> (forall n. (l <= n, n <= h) => NatRepr n -> a)
           -> [a]
natForEach l h f = natForEach' l h (\LeqProof LeqProof -> f)

-- | Recursor for natural numbeers.
natRec :: forall m f
       .  NatRepr m
       -> f 0
       -> (forall n. NatRepr n -> f n -> f (n + 1))
       -> f m
natRec n f0 ih = go n
  where
    go :: forall n'. NatRepr n' -> f n'
    go n' = case isZeroNat n' of
              ZeroNat    -> f0
              NonZeroNat -> let n'' = prior n' in ih n'' (go n'')

mulCancelR ::
  (1 <= c, (n1 * c) ~ (n2 * c)) => f1 n1 -> f2 n2 -> f3 c -> (n1 :~: n2)
mulCancelR _ _ _ = unsafeCoerce Refl

instance Eq (NatRepr m) where
    _ == _ = True

instance TestEquality NatRepr where
    testEquality (NatRepr m) (NatRepr n)
        | m == n = Just (unsafeCoerce Refl)
        | otherwise = Nothing


-- nat :: forall n. Witness n => TN.Natural
-- nat = TN.naturalFromInteger(natVal @n Proxy)

-- | Ordering over two distinct types with a proof they are equal.
data Ordering' x y where
    LT' :: Ordering' x y
    EQ' :: Ordering' x x
    GT' :: Ordering' x y
  
------------------------------------------------------------------------
-- Ordering
-- Requires PolyKinds
-- | A parameterized type that can be compared on distinct instances.
class TestEquality ktp => OrderEquality (ktp :: k -> *) where
    {-# MINIMAL compareF #-}

    -- | compareF compares two keys with different type parameters.
    -- Instances must ensure that keys are only equal if the type
    -- parameters are equal.
    compareF :: ktp x -> ktp y -> Ordering' x y

    leqF :: ktp x -> ktp y -> Bool
    leqF x y =
        case compareF x y of
            LT' -> True
            EQ' -> True
            GT' -> False

    ltF :: ktp x -> ktp y -> Bool
    ltF x y =
        case compareF x y of
            LT' -> True
            EQ' -> False
            GT' -> False

    geqF :: ktp x -> ktp y -> Bool
    geqF x y =
        case compareF x y of
            LT' -> False
            EQ' -> True
            GT' -> True

    gtF :: ktp x -> ktp y -> Bool
    gtF x y =
        case compareF x y of
            LT' -> False
            EQ' -> False
            GT' -> True


-- | Convert type-level Ordering' to value-level ordering.
toOrdering :: Ordering' x y -> Ordering
toOrdering LT' = LT
toOrdering EQ' = EQ
toOrdering GT' = GT

-- | Convert value-level 'Ordering' to type-level to 'Ordering''.
fromOrdering :: Ordering -> Ordering' x x
fromOrdering LT = LT'
fromOrdering EQ = EQ'
fromOrdering GT = GT'

-- Requires RankNTypes
-- | `joinOrderingF x y` first compares on x, returning an equivalent
-- value if it is not `EQF`.  If it is EQF, it returns `y`.
joinOrdering' :: forall (a :: j) (b :: j) (c :: k) (d :: k)
            .  Ordering' a b
            -> (a ~ b => Ordering' c d)
            -> Ordering' c d
joinOrdering' EQ' y = y
joinOrdering' LT' _ = LT'
joinOrdering' GT' _ = GT'