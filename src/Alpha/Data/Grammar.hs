{-# LANGUAGE DataKinds #-}
module Alpha.Data.Grammar where
import Alpha.Base
import Alpha.Canonical
import qualified Data.List as List
import Alpha.Text.Combinators
import Alpha.Data.Asci

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
        
product::[Atom a] -> [Atom a] -> [(Atom a,Atom a)]
product s1 s2 = [(x,y) | x <- s1, y <- s2 ]

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

instance Decomposable (Atoms a) (Atom a)  where
    decompose (Atoms x) = x

instance (Formattable a) => Formattable (Atom a) where
    format (Atom x) = format x

instance (Formattable a) =>  Formattable (Atoms a) where
    format (Atoms x) = embrace( fmap format x)
    
instance (Ord a, Concatenable a a) => Concatenable (Atom a) (Atom a) where    
    type Concatenated (Atom a) (Atom a) = Atoms a
    concat a1 a2 = Atoms [a1, a2]

instance Counted (Atoms a) where
    count (Atoms a) = (List.length a) |> fromIntegral

-- Defines an alphabet consisting of the digits 0-9    
digital :: Alphabet a String
digital = alphabet $ atoms [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]
    