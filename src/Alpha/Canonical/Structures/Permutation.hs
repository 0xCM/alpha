-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Canonical.Structures.Permutation
(
    module X,
    Permutation(..),
    ToPermutation(..),
    switch,
    symgroup
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Structures.NatK as X
import Alpha.Canonical.Structures.StructuredGroup as X

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Math.Combinat.Permutations as Combinat

default (Int, Double, Text)

type PermArray = UArray Int Int

type CombiPerm = Combinat.Permutation

-- Represents a bijective function over the set {1,2...,n}
newtype Permutation n = Permutation (Map Int Int)
    deriving(Eq,Generic,Functor,Ord)
instance Newtype(Permutation n)

newtype PermuMul n = PermuMul (O2 (Permutation n))
    deriving(Generic)
instance Newtype (PermuMul n)

type instance Dom (Permutation n) = Int
type instance Cod (Permutation n) = Int
type instance Individual (Permutation n) = Permutation n
type instance Individual (UArray i a) = a

type SymmetricGroup n = [Permutation n]

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class PermLookup p where
    image::p -> Int -> Int

-- | Characterizes types from which permutations can be constructed    
class (KnownNat n) => ToPermutation n a where
    permutation::a -> Permutation n


combiperm :: PermArray -> CombiPerm
combiperm = Combinat.uarrayToPermutationUnsafe

permarray::forall n. KnownNat n => Permutation n -> PermArray
permarray (Permutation map) = arr where
    ix0 = 1
    ix1 = natg @n
    pairs = [valOrDefault (map !? i) (i,i) | i <- [ix0..ix1]]
    arr = uarray (ix0,ix1) pairs

-- Constructs the symmetric group of degree n        
symgroup::forall n. KnownNat n => SymmetricGroup n
symgroup = sg where
    symbols = [1..nat @n]
    perms = List.permutations symbols
    sg = perms |> fmap (\p ->  (pairzip symbols p) ) 
               |> fmap (\p -> Map.fromList p) 
               |> fmap Permutation
       

-- | Effects a transposition
switch::forall n. KnownNat n => (Int,Int) -> Permutation n -> Permutation n
switch (i,j) p =  unwrap p |> toList |> fmap (\(r,s) -> rule (r,s) ) |> permutation 
    where
        rule::(Int,Int) -> (Int,Int)
        rule (r,s) =  if r == i then (r, image p j)
                      else if r == j then (r, image p i)   
                      else (r, s)

instance KnownNat n => Signable (Permutation n) where
    sign p = ifelse even Positive Negative where
        even = p |> permarray |> combiperm |> Combinat.isEvenPermutation 

instance KnownNat n => ToPermutation n [Int] where
    permutation range = Permutation  $ Map.fromList (pairzip pt range) 
        where                
            domain = natKspan @1 @n
            pt = int <$> individuals domain
                
instance KnownNat n => ToPermutation n [(Int,Int)] where
    permutation = Permutation . Map.fromList    

instance forall n. KnownNat n => Preimage (Permutation n) where
    preimage (Permutation p) i =  toList p |> filter (\(k,v) -> k == i) |> fmap(\(k,v) -> k) |> head
    
instance  KnownNat n => Multiplicative (Permutation n) where
    mul g (Permutation f) 
        = f |> Map.toList 
            |> fmap (\(i,j) -> (i, image g j)) 
            |> Map.fromList 
            |> Permutation
                                          
instance KnownNat n => Formattable (Permutation n) where
    format perm = rows where
        row1 = perm |> mappings |> fmap (\(x,y) -> format x) |> weave Blank |> fence Pipe Pipe
        row2 = perm |> mappings |> fmap (\(x,y) -> format y) |> weave Blank |> fence Pipe Pipe
        sep = Text.replicate 20 "-"
        rows = sep <> EOL <> row1 <> EOL <> row2

        mappings::forall n. KnownNat n => Permutation n -> [(Int,Int)] 
        mappings (Permutation p) = toList p
        
instance KnownNat n => Show (Permutation n) where
    show = string . format

instance forall n. KnownNat n =>  PermLookup (Permutation n ) where
    image (Permutation s ) i = s Map.! i

instance forall n. KnownNat n => Operator 2 (PermuMul n) (Permutation n)  where    
    operator = operation $ PermuMul (*)

    evaluate (f,g) = f * g 
    {-# INLINE evaluate #-}
    
instance  KnownNat n => Semigroup (Permutation n) where
    g <> f = g * f

instance forall n. KnownNat n =>  Unital (Permutation n) where
    one = permutation @n [1..(natg @n)] where
        
instance forall n.  KnownNat n => IdentityStructure (Permutation n) where
    sidentity = one
                                
instance forall n.  KnownNat n => InversionStructure (Permutation n) where
    sinvert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Monoid (Permutation n) where
    mempty = one