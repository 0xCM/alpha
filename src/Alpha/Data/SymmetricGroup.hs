module Alpha.Data.SymmetricGroup
(
    SymmetricGroup, symgroup
)
where
--import Alpha.Numerics.Base hiding(Unital, one, Invertible,Semigroup,Monoid,Group,Multiplicative)
import Alpha.Base hiding (Monoid,Semigroup)
import Alpha.Canonical hiding(Unital, one, Invertible,Semigroup,Monoid,Multiplicative)
import Alpha.Canonical.Structures 
import Alpha.Data.Permutation
import Alpha.Canonical.Common.Asci
import qualified Alpha.Canonical.Algebra as A

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map



-- Represents the set of all permutations on a set with n elements    
newtype SymmetricGroup n = SymmetricGroup [Permutation n]
    deriving (Generic, Eq)
instance Newtype (SymmetricGroup n)


type instance IndexedElement Int (SymmetricGroup n) = Permutation n
type instance Individual (SymmetricGroup n) = Permutation n

-- Constructs the symmetric group of degree n        
symgroup::forall n.KnownNat n => SymmetricGroup n
symgroup = sg where
    symbols = [1..nat @n]
    perms = List.permutations symbols
    sg = perms |> fmap (\perm ->  (List.zip symbols perm) ) 
               |> fmap (\perm -> Map.fromList perm) 
               |> fmap Permutation
               |> SymmetricGroup

            
instance forall n. KnownNat n => Formattable (SymmetricGroup n) where
    format (SymmetricGroup sg) 
        = [Su, n, Colon, EOL, items] |> Text.concat  where        
            n = format (nat @n) 
            items = sg |> fmap (\p -> format p) |> format

instance forall n. KnownNat n => Indexed Int (SymmetricGroup n) where
    at sg i = (unwrap sg) List.!! i

instance forall n. KnownNat n => Finite (SymmetricGroup n) where
    count _ = factorial (nat @n) |> fromIntegral
            
instance forall n. KnownNat n => Show (SymmetricGroup n) where
    show = string . format 
            
instance forall n. KnownNat n => Unital (SymmetricGroup n) where
    one::Permutation n
    one = A.one
        
-- instance forall n .KnownNat n => Invertible (SymmetricGroup n) where
--     invert::Permutation n -> Permutation n
--     invert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Multiplicative (SymmetricGroup n) where
    g * f  = g <> f


instance forall n. KnownNat n  => SetBuilder (SymmetricGroup n) where
    set (SymmetricGroup p)  = fneSet $ (head p) :| tail p

instance forall n. KnownNat n => Semigroup (SymmetricGroup n)    
    
instance forall n. KnownNat n => Monoid (SymmetricGroup n)
    
--instance forall n. KnownNat n => Group (SymmetricGroup n)

--instance forall n. KnownNat n => DiscreteGroup (SymmetricGroup n) (Permutation n) where
