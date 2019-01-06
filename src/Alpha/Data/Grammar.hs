{-# LANGUAGE DataKinds #-}
module Alpha.Data.Grammar
(
    Atom(..), 
    Atoms(..), 
    Alphabet(..), 
    Phrase(..),
    atom,
    atoms,
    alphabet,
    phrase
)
where

import qualified Data.List as List
import Alpha.Canonical

import qualified Alpha.Canonical.Common.Asci as Asci

type instance Individual (Atom a) = a
type instance Individual (Atoms a) = Atom a


-- An indivisible constituent
newtype Atom a = Atom a
    deriving(Eq, Show, Data, Typeable, Ord)

-- / A juxtapostion of a finite number of atoms    
newtype Atoms a = Atoms [Atom a]
    deriving(Eq, Show, Data, Typeable, Ord)

-- | An 'Alphabet' is a finite set of symbols with at least two elements
-- Here, a represents a domain-specific type that can be used to classify a particular
-- 'Alphabet' instantiation
data Alphabet b a = Alphabet Int (Atoms a)
    deriving(Eq, Show, Ord, Data, Typeable)

-- | A 'Phrase' of length k over an alphabet A is a k-tuple of elements of A
-- or the concatenation of such elements
data Phrase a (k::Nat) = Phrase Int (Atoms a)
    deriving(Eq, Show, Ord, Data, Typeable)

-- Constructs a 'Symbol' value from a 'Char' value
atom::(Ord a) => a -> Atom a
atom x = Atom x

-- Constructs an 'Atom' coollection
atoms::(Ord a) => [a] -> Atoms a
atoms xs = Atoms(fmap atom xs)

-- | Constructs an alphabet from from list of atoms
alphabet::Atoms a -> Alphabet b a
alphabet x = Alphabet (count x) x

-- | Constructs a 'Phrase' from a list of 'Symbol' values
phrase::Atoms a -> Phrase a k
phrase input = Phrase (count input) input

-- Defines an alphabet consisting of the digits 0-9    
digital :: Alphabet a Text
digital = alphabet $ atoms ["0","1","2","3","4","5","6","7","8","9"]

instance Componentized (Atoms a) where
    components (Atoms x) = x

instance (Formattable a) => Formattable (Atom a) where
    format (Atom x) = format x

instance (Formattable a) =>  Formattable (Atoms a) where
    format (Atoms x) = embrace( fmap format x)
    
instance (Ord a, Concatenable a a) => Concatenable (Atom a) (Atom a) where        
    type Concatenated (Atom a) (Atom a) = Atoms a    
    concat a1 a2 = Atoms [a1, a2]

instance Discrete (Atoms a) where    
    individuals (Atoms a) = a
instance Finite (Atoms a)
    