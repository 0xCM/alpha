-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Alpha.Data.Grammar
(
    Atom(..), 
    Alphabet(..), 
    Phrase(..),
    atom,
    alphabet,
    phrase
)
where

import qualified Data.List as List
import Alpha.Canonical

import qualified Alpha.Canonical.Common.Asci as Asci

type instance Individual (Atom a) = a
type instance Individual (Alphabet a) = Atom a
type instance Individual (Phrase a) = Atom a

-- An indivisible constituent
newtype Atom a = Atom a
    deriving(Eq, Show, Data, Typeable, Ord)

-- | A collection of atoms frm which phrases may be formed
newtype Alphabet a = Alphabet (FiniteSet (Atom a)) 
    deriving(Eq, Show, Ord, Data, Typeable)

-- | A sequence of adjacent atoms    
newtype Phrase a = Phrase [Atom a]
    deriving(Eq, Show, Ord, Data, Typeable)

-- Constructs a 'Symbol' value from a 'Char' value
atom::(Ord a) => a -> Atom a
atom = Atom 

-- | Constructs an alphabet from from list of atoms
alphabet::(Ord a) => [Atom a] -> Alphabet a
alphabet atoms = Alphabet (set atoms)

-- | Constructs a 'Phrase' from a list of 'Symbol' values
phrase::[Atom a] -> Phrase a 
phrase = Phrase

-- Defines an alphabet consisting of the digits 0-9    
digital :: Alphabet Text
digital = alphabet $ atom <$> asciD

instance (Ord a, Nullary a) => Nullary (Atom a) where
    zero = atom zero

instance (Formattable a) => Formattable (Atom a) where
    format (Atom x) = format x

instance (Formattable a) =>  Formattable (Phrase a) where
    format (Phrase x) = embrace( fmap format x)
    
instance (Ord a, BiConcatenable a a) => BiConcatenable (Atom a) (Atom a) where        
    type BiConcatenated (Atom a) (Atom a) = Phrase a    
    biconcat a1 a2 = Phrase [a1, a2]

instance Discrete (Phrase a) where  
    individuals (Phrase a) = a

instance FinitelyCountable (Phrase a) where
    count (Phrase atoms) = length atoms
    