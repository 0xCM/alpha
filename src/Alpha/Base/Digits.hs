-----------------------------------------------------------------------------
-- | Provides digit manipulation operations
-- Adapted from: https://bitbucket.org/sffubs/digits
-- Copyright   : Original author and Chris Moore(c) 0xCM, 2018
-- License     :  BSD/MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Base.Digits 
(
    
) where

import GHC.Real
import GHC.Natural(Natural)
import GHC.TypeLits
import GHC.Show
import Data.Eq
import Data.Ord
import Data.Foldable
import GHC.Num
import Data.Semigroup
import Data.Function
import Data.Functor
import Data.Proxy
import Prelude(error)
import qualified Data.List as List

-- Represents the numeric base
newtype NumberBase n = NumberBase n
    deriving(Show,Eq,Ord,Num)

newtype Digits (b::Nat) n = Digits [n]

base::(Integral n) => n -> NumberBase n
base = NumberBase

-- | Returns the digits of a positive integer as a Maybe list, in reverse order
--   or Nothing if a zero or negative base is given
--   This is slightly more efficient than in forward order.
mDigitsRev::Integral n => NumberBase n -> n -> [n] 
mDigitsRev (NumberBase base) i = if base < 1
                    then error "Base must be >= 1"
                    else dr base i
    where
        dr _ 0 = []
        dr b x = case base of
                1 -> List.genericTake x $ List.repeat 1
                _ -> let (rest, lastDigit) = quotRem x b
                        in lastDigit : dr b rest

-- | Returns the digits of a positive integer as a Maybe list.
--   or Nothing if a zero or negative base is given
mDigits::Integral n => NumberBase n  -> n -> [n]
mDigits base i = List.reverse (mDigitsRev base i)

-- | Returns the digits of a positive integer as a list, in reverse order.
--   Throws an error if given a zero or negative base.
digitsRev :: Integral n => NumberBase n -> n -> [n]
digitsRev base = mDigitsRev base

-- | Returns the digits of a positive integer as a list.
--   Throws an error if given a zero or negative base.
digits::forall b n. (KnownNat b, Integral n) => n -> Digits b n
digits n = Digits $ digits' base n where
    base = fromIntegral  (natVal (Proxy :: Proxy b))

    digits' :: Integral n => NumberBase n -> n -> [n] 
    digits' base = List.reverse . digitsRev base

    
-- | Takes a list of digits, and converts them back into a positive integer.
unDigits'::Integral n => NumberBase n -> [n] -> n   -- ^ The original number.
unDigits' (NumberBase base) = foldl (\ a b -> a * base + b) 0

undigits::forall b n. (KnownNat b, Integral n) => Digits b n -> n 
undigits (Digits d) = unDigits' base d where
    base = NumberBase $ fromIntegral  (natVal (Proxy :: Proxy b))
    
instance forall b n. (KnownNat b, Show n) => Show (Digits b n) where
    show (Digits n) = val <> " (" <> b <> ")" where
        val = List.concat $ show <$> n 
        b = show $ natVal (Proxy :: Proxy b)     
