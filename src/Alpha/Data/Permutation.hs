{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Data.Permutation
(
    Permutation(..),
    perm,
    switch

)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Collective
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Partition
import Alpha.Canonical.Common.Asci
import Alpha.Canonical.Algebra.Naturals

import qualified Data.Text as Text
import Prelude(snd)
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Map as Map


default (Int, Double, Text)

type PermutationMap = Map Int Int

-- Represents a bijective function over the set {1,2...,n}
newtype Permutation n = Permutation PermutationMap
    deriving(Eq,Generic,Functor,Ord)
instance Newtype(Permutation n)

type instance Dom (Permutation n) = Int
type instance Cod (Permutation n) = Int
type instance IndexedElement Int (Permutation n) = Int
type instance Individual (Permutation n) = Permutation n


mappings::forall n. KnownNat n => Permutation n -> [(Int,Int)] 
mappings (Permutation p) = toList p

-- | Constructs a permutation of lenth n from an explicit list of correspondences
perm::forall n. KnownNat n => [(Int,Int)] -> Permutation n
perm = Permutation . Map.fromList

image::forall n. KnownNat n => Permutation n -> Int -> Int
image = (!)

preimage::forall n. KnownNat n =>Permutation n -> Int -> Int
preimage (Permutation p) i =  toList p |> filter (\(k,v) -> k == i) |> fmap(\(k,v) -> k) |> head

-- | Effects a transposition
switch::forall n. KnownNat n => (Int,Int) -> Permutation n -> Permutation n
switch (i,j) p =  unwrap p |> toList |> fmap (\(r,s) -> rule (r,s) ) |> perm 
    where
        rule::(Int,Int) -> (Int,Int)
        rule (r,s) =  if r == i then (r, p ! j)
                      else if r == j then (r, p ! i)   
                      else (r, s)

instance KnownNat n => Formattable (Permutation n) where
    format perm = rows where
        row1 = perm |> mappings |> fmap (\(x,y) -> format x) |> weave Space |> enclose "|" "|"
        row2 = perm |> mappings |> fmap (\(x,y) -> format y) |> weave Space |> enclose "|" "|"
        sep = Text.replicate 20 "-"
        rows = sep <> EOL <> row1 <> EOL <> row2

instance KnownNat n => Show (Permutation n) where
    show = string . format

instance forall n a.KnownNat n =>  Indexed Int (Permutation n) where
    at (Permutation s ) i = s Map.! i

newtype PermuMul n = PermuMul (O2 (Permutation n))
    deriving(Generic)
instance Newtype (PermuMul n)

instance forall n. KnownNat n => Operator 2 (PermuMul n) (Permutation n)  where    
    operator = operation $ PermuMul permumul

    evaluate (f,g) = permumul f g 
    {-# INLINE evaluate #-}

    

permumul::KnownNat n => O2 (Permutation n)
permumul g (Permutation f) = f |> Map.toList 
                    |> fmap (\(i,j) -> (i, g ! j)) 
                    |> Map.fromList 
                    |> Permutation

instance  KnownNat n => Multiplicative (Permutation n) where
    mul = permumul

instance  KnownNat n => Semigroup (Permutation n) where
    g <> f = g * f

instance forall n. KnownNat n =>  Unital (Permutation n) where
    one = permutation @n [minBound..maxBound] where
        n = int $ nat @n

        -- Defines a permutation domain consisting of the integers 1..n
        domain::forall n. KnownNat n => NatKSpan 1 n
        domain = natKspan @1 @n

        permutation::forall n. KnownNat n => [Int] -> Permutation n
        permutation range = Permutation  z 
            where        
                s = natKspan @1 @n
                d = domain @n
                pt = int <$> breakpoints s 
                z =  Map.fromList (List.zip pt range)

                
-- instance (forall n a. KnownNat n) => Invertible (Permutation n) where
--     invert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Monoid (Permutation n) where
    mempty = one