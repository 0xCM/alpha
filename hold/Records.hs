-----------------------------------------------------------------------------
-- | Utilties to facilitate record manipulation
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Data.Records
(
    Record1(..), Record2(..), Record3(..), Record4(..), Record5(..),
    Record6(..), Record7(..), Record8(..), Record9(..),
    type (.*), type (*.),
    Recorder(..),
    RecordBuilder(..),
    Fielded(..)
)
where
import Alpha.Base
import Alpha.Data.Product
import Alpha.Canonical

-- | Canonical record with 1 field
data Record1 a1 = Record1 {
    f1::a1
    } deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 2 fields 
data Record2 a1 a2 = Record2 { 
    f1::a1, f2::a2
    } deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 3 fields     
data Record3 a1 a2 a3 = Record3 {
    f1::a1, f2::a2, f3::a3
    } deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 4 fields     
data Record4 a1 a2 a3 a4 = Record4 {
    f1::a1, f2::a2, f3::a3, f4::a4
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 5 fields 
data Record5 a1 a2 a3 a4 a5 = Record5 {
    f1::a1, f2::a2, f3::a3, f4::a4, f5::a5
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 6 fields 
data Record6 a1 a2 a3 a4 a5 a6 = Record6 {
    f1::a1, f2::a2, f3::a3, f4::a4, f5::a5,
    f6::a6
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 7 fields 
data Record7 a1 a2 a3 a4 a5 a6 a7 = Record7 {
    f1::a1, f2::a2, f3::a3, f4::a4, f5::a5,
    f6::a6, f7::a7
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 8 fields 
data Record8 a1 a2 a3 a4 a5 a6 a7 a8 = Record8 {
    f1::a1, f2::a2, f3::a3, f4::a4, f5::a5,
    f6::a6, f7::a7, f8::a8
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

-- | Canonical record with 9 fields 
data Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Record9 {
    f1::a1, f2::a2, f3::a3, f4::a4, f5::a5,
    f6::a6, f7::a7, f8::a8, f9::a9
} deriving(Eq, Ord, Data, Generic, Typeable, Show, Read)

type family a .* b = r | r -> a b where
    Record1 a1 .* b = Record2 a1 b
    Record2 a1 a2 .* b = Record3 a1 a2 b
    Record3 a1 a2 a3 .* b = Record4 a1 a2 a3 b
    Record4 a1 a2 a3 a4 .* b = Record5 a1 a2 a3 a4 b
    Record5 a1 a2 a3 a4 a5 .* b = Record6 a1 a2 a3 a4 a5 b
    Record6 a1 a2 a3 a4 a5 a6 .* b = Record7 a1 a2 a3 a4 a5 a6 b
    Record7 a1 a2 a3 a4 a5 a6 a7 .* b = Record8 a1 a2 a3 a4 a5 a6 a7 b
    Record8 a1 a2 a3 a4 a5 a6 a7 a8 .* b = Record9 a1 a2 a3 a4 a5 a6 a7 a8 b

infixl 5 .*

type family a *. b = r | r -> a b where
    b *. Record1 a1 = Record2 b a1
    b *. Record2 a1 a2 = Record3 b a1 a2
    b *. Record3 a1 a2 a3 = Record4 b a1 a2 a3
    b *. Record4 a1 a2 a3 a4 = Record5 b a1 a2 a3 a4
    b *. Record5 a1 a2 a3 a4 a5 = Record6 b a1 a2 a3 a4 a5
    b *. Record6 a1 a2 a3 a4 a5 a6 = Record7 b a1 a2 a3 a4 a5 a6
    b *. Record7 a1 a2 a3 a4 a5 a6 a7 = Record8 b a1 a2 a3 a4 a5 a6 a7
    b *. Record8 a1 a2 a3 a4 a5 a6 a7 a8 = Record9 b a1 a2 a3 a4 a5 a6 a7 a8

infixl 5 *.

class RecordBuilder r where
    (.*)::r -> b -> r .* b
    (*.)::b -> r -> b *. r

instance RecordBuilder (Record1 a1) where    
    (Record1 a1) .* b  = record (a1, b)
    b *. (Record1 a1) = record (b, a1)

instance RecordBuilder (Record2 a1 a2) where    
    (Record2 a1 a2) .* b  = record (a1, a2, b)
    b *. (Record2 a1 a2)  = record (b, a1, a2)

instance RecordBuilder (Record3 a1 a2 a3) where    
    (Record3 a1 a2 a3) .* b  = record (a1, a2, a3, b)
    b *. (Record3 a1 a2 a3)  = record (b, a1, a2, a3)

instance RecordBuilder (Record4 a1 a2 a3 a4) where    
    (Record4 a1 a2 a3 a4) .* b  = record (a1, a2, a3, a4, b)
    b *. (Record4 a1 a2 a3 a4)  = record (b, a1, a2, a3, a4)

instance RecordBuilder (Record5 a1 a2 a3 a4 a5) where    
    (Record5 a1 a2 a3 a4 a5) .* b  = record (a1, a2, a3, a4, a5, b)
    b *. (Record5 a1 a2 a3 a4 a5)  = record (b, a1, a2, a3, a4, a5)

instance RecordBuilder (Record6 a1 a2 a3 a4 a5 a6) where    
    (Record6 a1 a2 a3 a4 a5 a6) .* b  = record (a1, a2, a3, a4, a5, a6, b)
    b *. (Record6 a1 a2 a3 a4 a5 a6)  = record (b, a1, a2, a3, a4, a5, a6)

instance RecordBuilder (Record7 a1 a2 a3 a4 a5 a6 a7) where    
    (Record7 a1 a2 a3 a4 a5 a6 a7) .* b  = record (a1, a2, a3, a4, a5, a6, a7, b)
    b *. (Record7 a1 a2 a3 a4 a5 a6 a7)  = record (b, a1, a2, a3, a4, a5, a6, a7)

instance RecordBuilder (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where    
    (Record8 a1 a2 a3 a4 a5 a6 a7 a8) .* b  = record (a1, a2, a3, a4, a5, a6, a7, a8, b)
    b *. (Record8 a1 a2 a3 a4 a5 a6 a7 a8)  = record (b, a1, a2, a3, a4, a5, a6, a7, a8)
                            
-- | Arity-polymorphic record type
type family Record  a = r where
    Record (Product1 a1) = (Record1 a1 )        
    
    Record (Product2 a1 a2) = Record2 a1 a2
    Record (a1,a2) = Record2 a1 a2

    Record (Product3 a1 a2 a3) = Record3 a1 a2 a3
    Record (a1,a2,a3) = Record3 a1 a2 a3

    Record (Product4 a1 a2 a3 a4) = Record4 a1 a2 a3 a4
    Record (a1,a2,a3,a4) = Record4 a1 a2 a3 a4

    Record (Product5 a1 a2 a3 a4 a5) = Record5 a1 a2 a3 a4 a5
    Record (a1,a2,a3,a4,a5) = Record5 a1 a2 a3 a4 a5

    Record (Product6 a1 a2 a3 a4 a5 a6) = Record6 a1 a2 a3 a4 a5 a6
    Record (a1,a2,a3,a4,a5,a6) = Record6 a1 a2 a3 a4 a5 a6

    Record (Product7 a1 a2 a3 a4 a5 a6 a7) = Record7 a1 a2 a3 a4 a5 a6 a7
    Record (a1,a2,a3,a4,a5,a6,a7) = Record7 a1 a2 a3 a4 a5 a6 a7

    Record (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = Record8 a1 a2 a3 a4 a5 a6 a7 a8
    Record (a1,a2,a3,a4,a5,a6,a7,a8) = Record8 a1 a2 a3 a4 a5 a6 a7 a8

    Record (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    Record (a1,a2,a3,a4,a5,a6,a7,a8,a9) = Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9

class Recorder a where        
    record::a -> Record a


instance Recorder (a1,a2) where
    record (a1,a2) = Record2 {f1 = a1, f2 = a2}
    
instance Recorder (a1,a2,a3) where
    record (a1,a2,a3) = Record3 {f1 = a1, f2 = a2,f3 = a3}
instance Recorder (a1,a2,a3,a4) where
    record (a1,a2,a3,a4) = Record4 {f1 = a1, f2 = a2,f3 = a3,f4 = a4}        
instance Recorder (a1,a2,a3,a4,a5) where
    record (a1,a2,a3,a4,a5) = Record5 {f1 = a1, f2 = a2,f3 = a3,f4 = a4,f5 = a5}
instance Recorder (a1,a2,a3,a4,a5,a6) where
    record (a1,a2,a3,a4,a5,a6) = Record6 {f1 = a1, f2 = a2,f3 = a3,f4 = a4,f5 = a5,f6 = a6}
instance Recorder (a1,a2,a3,a4,a5,a6,a7) where
    record (a1,a2,a3,a4,a5,a6,a7) = Record7 {f1 = a1, f2 = a2,f3 = a3,f4 = a4,f5 = a5,f6 = a6, f7=a7}
instance Recorder (a1,a2,a3,a4,a5,a6,a7,a8) where
    record (a1,a2,a3,a4,a5,a6,a7,a8) = Record8 {f1 = a1, f2 = a2,f3 = a3,f4 = a4,f5 = a5,f6 = a6, f7=a7,f8= a8}
instance Recorder (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    record (a1,a2,a3,a4,a5,a6,a7,a8,a9) = Record9 {f1 = a1, f2 = a2,f3 = a3,f4 = a4,f5 = a5,f6 = a6, f7=a7,f8= a8, f9 = a9}
                            
instance Recorder (Product1 a1) where
    record (Product1 a1) = Record1 {f1 = a1}
instance Recorder (Product2 a1 a2) where
    record (Product2 a1 a2) = Record2{ f1 = a1, f2 = a2}
instance Recorder (Product3 a1 a2 a3) where
    record (Product3 a1 a2 a3) = Record3{ f1 = a1, f2 = a2, f3 = a3}
instance Recorder (Product4 a1 a2 a3 a4) where
    record (Product4 a1 a2 a3 a4) = Record4{ f1 = a1, f2 = a2, f3 = a3, f4 = a4}
instance Recorder (Product5 a1 a2 a3 a4 a5) where
    record (Product5 a1 a2 a3 a4 a5) = Record5{ f1 = a1, f2 = a2, f3 = a3, f4 = a4, f5 = a5}
instance Recorder (Product6 a1 a2 a3 a4 a5 a6) where
    record (Product6 a1 a2 a3 a4 a5 a6) = Record6{ f1 = a1, f2 = a2, f3 = a3, f4 = a4, f5 = a5, f6 = a6}
instance Recorder (Product7 a1 a2 a3 a4 a5 a6 a7) where
    record (Product7 a1 a2 a3 a4 a5 a6 a7) = Record7{ f1 = a1, f2 = a2, f3 = a3, f4 = a4, f5 = a5, f6 = a6, f7= a7}
instance Recorder (Product8 a1 a2 a3 a4 a5 a6 a7 a8) where
    record (Product8 a1 a2 a3 a4 a5 a6 a7 a8) = Record8{ f1 = a1, f2 = a2, f3 = a3, f4 = a4, f5 = a5, f6 = a6, f7= a7, f8 = a8}
instance Recorder (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where
    record (Product9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Record9{ f1 = a1, f2 = a2, f3 = a3, f4 = a4, f5 = a5, f6 = a6, f7= a7, f8 = a8, f9 = a9}

data Name (s::Symbol) = Name

name::forall (s::Symbol). Name s
name = Name
    
type family NamedField (j::Nat) s a  where
    NamedField 1 s (Record2 a1 a2) = (Name s,a1)
    NamedField 2 s (Record2 a1 a2) = (Name s,a2)
    
    NamedField 1 s (Record3 a1 a2 a3) = (Name s,a1)
    NamedField 2 s (Record3 a1 a2 a3) = (Name s,a2)
    NamedField 3 s (Record3 a1 a2 a3) = (Name s,a3)

    NamedField 1 s (Record4 a1 a2 a3 a4) = (Name s,a1)
    NamedField 2 s (Record4 a1 a2 a3 a4) = (Name s,a2)
    NamedField 3 s (Record4 a1 a2 a3 a4) = (Name s,a3)
    NamedField 4 s (Record4 a1 a2 a3 a4) = (Name s,a4)

    NamedField 1 s (Record5 a1 a2 a3 a4 a5) = (Name s,a1)
    NamedField 2 s (Record5 a1 a2 a3 a4 a5) = (Name s,a2)
    NamedField 3 s (Record5 a1 a2 a3 a4 a5) = (Name s,a3)
    NamedField 4 s (Record5 a1 a2 a3 a4 a5) = (Name s,a4)
    NamedField 5 s (Record5 a1 a2 a3 a4 a5) = (Name s,a5)

    NamedField 1 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a1)
    NamedField 2 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a2)
    NamedField 3 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a3)
    NamedField 4 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a4)
    NamedField 5 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a5)
    NamedField 6 s (Record6 a1 a2 a3 a4 a5 a6) = (Name s,a6)

    NamedField 1 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a1)
    NamedField 2 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a2)
    NamedField 3 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a3)
    NamedField 4 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a4)
    NamedField 5 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a5)
    NamedField 6 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a6)
    NamedField 7 s (Record7 a1 a2 a3 a4 a5 a6 a7) = (Name s,a7)

    NamedField 1 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a1)
    NamedField 2 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a2)
    NamedField 3 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a3)
    NamedField 4 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a4)
    NamedField 5 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a5)
    NamedField 6 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a6)
    NamedField 7 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a7)
    NamedField 8 s (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = (Name s,a8)

    NamedField 1 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a1)
    NamedField 2 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a2)
    NamedField 3 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a3)
    NamedField 4 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a4)
    NamedField 5 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a5)
    NamedField 6 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a6)
    NamedField 7 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a7)
    NamedField 8 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a8)
    NamedField 9 s (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = (Name s,a9)

-- | Arity-polymorphic field type    
type family Field (j::Nat) a where
    Field 1 (Record2 a1 a2) = a1
    Field 2 (Record2 a1 a2) = a2

    Field 1 (Record3 a1 a2 a3) = a1
    Field 2 (Record3 a1 a2 a3) = a2
    Field 3 (Record3 a1 a2 a3) = a3

    Field 1 (Record4 a1 a2 a3 a4) = a1
    Field 2 (Record4 a1 a2 a3 a4) = a2
    Field 3 (Record4 a1 a2 a3 a4) = a3
    Field 4 (Record4 a1 a2 a3 a4) = a4

    Field 1 (Record5 a1 a2 a3 a4 a5) = a1
    Field 2 (Record5 a1 a2 a3 a4 a5) = a2
    Field 3 (Record5 a1 a2 a3 a4 a5) = a3
    Field 4 (Record5 a1 a2 a3 a4 a5) = a4
    Field 5 (Record5 a1 a2 a3 a4 a5) = a5

    Field 1 (Record6 a1 a2 a3 a4 a5 a6) = a1
    Field 2 (Record6 a1 a2 a3 a4 a5 a6) = a2
    Field 3 (Record6 a1 a2 a3 a4 a5 a6) = a3
    Field 4 (Record6 a1 a2 a3 a4 a5 a6) = a4
    Field 5 (Record6 a1 a2 a3 a4 a5 a6) = a5
    Field 6 (Record6 a1 a2 a3 a4 a5 a6) = a6

    Field 1 (Record7 a1 a2 a3 a4 a5 a6 a7) = a1
    Field 2 (Record7 a1 a2 a3 a4 a5 a6 a7) = a2
    Field 3 (Record7 a1 a2 a3 a4 a5 a6 a7) = a3
    Field 4 (Record7 a1 a2 a3 a4 a5 a6 a7) = a4
    Field 5 (Record7 a1 a2 a3 a4 a5 a6 a7) = a5
    Field 6 (Record7 a1 a2 a3 a4 a5 a6 a7) = a6
    Field 7 (Record7 a1 a2 a3 a4 a5 a6 a7) = a7

    Field 1 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a1
    Field 2 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a2
    Field 3 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a3
    Field 4 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a4
    Field 5 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a5
    Field 6 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a6
    Field 7 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a7
    Field 8 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) = a8

    Field 1 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a1
    Field 2 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a2
    Field 3 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a3
    Field 4 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a4
    Field 5 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a5
    Field 6 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a6
    Field 7 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a7
    Field 8 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a8
    Field 9 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) = a9


class Fielded (j::Nat) a where    
    field::a -> Field j a

-- | Extracts a field from a record
field' :: r -> (r -> f) -> f 
field' = flip ($)
    
instance Fielded 1 (Record2 a1 a2) where 
    field r = field' @(Record2 a1 a2) r f1
instance Fielded 2 (Record2 a1 a2) where  
    field r = field' @(Record2 a1 a2) r f2

instance Fielded 1 (Record3 a1 a2 a3) where 
    field r = field' @(Record3 a1 a2 a3) r f1
instance Fielded 2 (Record3 a1 a2 a3) where  
    field r = field' @(Record3 a1 a2 a3) r f2
instance Fielded 3 (Record3 a1 a2 a3) where  
    field r = field' @(Record3 a1 a2 a3) r f3
        
instance Fielded 1 (Record4 a1 a2 a3 a4) where 
    field r = field' @(Record4 a1 a2 a3 a4) r f1
instance Fielded 2 (Record4 a1 a2 a3 a4) where  
    field r = field' @(Record4 a1 a2 a3 a4) r f2
instance Fielded 3 (Record4 a1 a2 a3 a4) where  
    field r = field' @(Record4 a1 a2 a3 a4) r f3
instance Fielded 4 (Record4 a1 a2 a3 a4) where  
    field r = field' @(Record4 a1 a2 a3 a4) r f4

instance Fielded 1 (Record5 a1 a2 a3 a4 a5) where 
    field r = field' @(Record5 a1 a2 a3 a4 a5) r f1
instance Fielded 2 (Record5 a1 a2 a3 a4 a5) where  
    field r = field' @(Record5 a1 a2 a3 a4 a5) r f2
instance Fielded 3 (Record5 a1 a2 a3 a4 a5) where  
    field r = field' @(Record5 a1 a2 a3 a4 a5) r f3
instance Fielded 4 (Record5 a1 a2 a3 a4 a5) where  
    field r = field' @(Record5 a1 a2 a3 a4 a5) r f4
instance Fielded 5 (Record5 a1 a2 a3 a4 a5) where  
    field r = field' @(Record5 a1 a2 a3 a4 a5) r f5
            
instance Fielded 1 (Record6 a1 a2 a3 a4 a5 a6) where 
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f1
instance Fielded 2 (Record6 a1 a2 a3 a4 a5 a6) where  
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f2
instance Fielded 3 (Record6 a1 a2 a3 a4 a5 a6) where  
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f3
instance Fielded 4 (Record6 a1 a2 a3 a4 a5 a6) where  
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f4
instance Fielded 5 (Record6 a1 a2 a3 a4 a5 a6) where  
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f5    
instance Fielded 6 (Record6 a1 a2 a3 a4 a5 a6) where  
    field r = field' @(Record6 a1 a2 a3 a4 a5 a6) r f6

instance Fielded 1 (Record7 a1 a2 a3 a4 a5 a6 a7) where 
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f1
instance Fielded 2 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f2
instance Fielded 3 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f3
instance Fielded 4 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f4
instance Fielded 5 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f5    
instance Fielded 6 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f6
instance Fielded 7 (Record7 a1 a2 a3 a4 a5 a6 a7) where  
    field r = field' @(Record7 a1 a2 a3 a4 a5 a6 a7) r f7

instance Fielded 1 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f1
instance Fielded 2 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f2
instance Fielded 3 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f3
instance Fielded 4 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f4
instance Fielded 5 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f5    
instance Fielded 6 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f6
instance Fielded 7 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f7
instance Fielded 8 (Record8 a1 a2 a3 a4 a5 a6 a7 a8) where  
    field r = field' @(Record8 a1 a2 a3 a4 a5 a6 a7 a8) r f8

instance Fielded 1 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where 
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f1
instance Fielded 2 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f2
instance Fielded 3 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f3
instance Fielded 4 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f4
instance Fielded 5 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f5    
instance Fielded 6 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f6
instance Fielded 7 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f7
instance Fielded 8 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f8
instance Fielded 9 (Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) where  
    field r = field' @(Record9 a1 a2 a3 a4 a5 a6 a7 a8 a9) r f9
            
