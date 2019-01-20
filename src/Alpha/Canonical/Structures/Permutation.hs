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
    symmetries
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Structures.NatK as X
import Alpha.Canonical.Structures.Group as X

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Map as Map

default (Int, Double, Text)

type PermutationMap = Map Int Int

-- Represents a bijective function over the set {1,2...,n}
newtype Permutation n = Permutation PermutationMap
    deriving(Eq,Generic,Functor,Ord)
instance Newtype(Permutation n)

newtype PermuMul n = PermuMul (O2 (Permutation n))
    deriving(Generic)
instance Newtype (PermuMul n)

type instance Dom (Permutation n) = Int
type instance Cod (Permutation n) = Int
type instance Individual (Permutation n) = Permutation n

type SymmetricGroup n = [Permutation n]

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class PermLookup p where
    image::p -> Int -> Int

-- | Characterizes types from which permutations can be constructed    
class (KnownNat n) => ToPermutation n a where
    permutation::a -> Permutation n

-- Constructs the symmetric group of degree n        
symmetries::forall n. KnownNat n => SymmetricGroup n
symmetries = sg where
    symbols = [1..nat @n]
    perms = List.permutations symbols
    sg = perms |> fmap (\p ->  (List.zip symbols p) ) 
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

instance KnownNat n => ToPermutation n [Int] where
    permutation range = Permutation  $ Map.fromList (List.zip pt range) 
        where                
            domain = natKspan @1 @n
            pt = int <$> associates (domain)
                
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
        
instance forall n.  KnownNat n => Identity (Permutation n) where
    identity = one
                                
instance forall n.  KnownNat n => Inverter (Permutation n) where
    invert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Monoid (Permutation n) where
    mempty = one