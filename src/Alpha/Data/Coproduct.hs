{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Coproduct
(
    Coproduct, Coproductive(..)
    
)
where
import Alpha.Base

type family Coproduct a | a -> a

class Coproductive (m::Nat) (n::Nat) a b where
    -- | Injects an a-value into a b-valued coproduct
    coproduct::a -> Coproduct b

    -- | Selects an a-value from a b-valued coproduct if it exists
    cofactor::Coproduct b -> Maybe a

-- Coproduct 1         
data Sum1 a1 
    = Sum11 {-# UNPACK #-} !a1
    deriving (Eq, Ord, Data, Generic, Typeable, Show)
type instance Coproduct (Sum1 a1) = Sum1 a1

instance Coproductive 1 1 a1 (Sum1 a1 )  where 
    coproduct  = Sum11    
    cofactor (Sum11 a) = Just a

-- Coproduct 2     
-------------------------------------------------------------------------------
data Sum2 a1 a2 
     = Sum21 {-# UNPACK #-} !a1
     | Sum22 {-# UNPACK #-} !a2
     deriving (Eq, Ord, Data, Generic, Typeable, Show)

type instance Coproduct (Sum2 a1 a2) = Sum2 a1 a2     

instance Coproductive 2 1 a1 (Sum2 a1 a2)  where 
    coproduct  = Sum21    
    cofactor (Sum21 a) = Just a
    cofactor _         = Nothing

instance Coproductive 2 2 a2 (Sum2 a1 a2)  where 
    coproduct  = Sum22
    cofactor (Sum22 a) = Just a
    cofactor _         = Nothing


-- Coproduct 3
-------------------------------------------------------------------------------
data Sum3 a1 a2 a3 
    = Sum31 {-# UNPACK #-} !a1
    | Sum32 {-# UNPACK #-} !a2
    | Sum33 {-# UNPACK #-} !a3
    deriving (Eq, Ord, Data, Generic, Typeable, Show)


type instance Coproduct (Sum3 a1 a2 a3) = Sum3 a1 a2 a3

instance Coproductive 3 1 a1 (Sum3 a1 a2 a3)  where 
    coproduct  = Sum31
    cofactor (Sum31 a) = Just a
    cofactor _         = Nothing


instance Coproductive 3 2 a2 (Sum3 a1 a2 a3)  where 
    coproduct  = Sum32
    cofactor (Sum32 a) = Just a
    cofactor _         = Nothing


instance Coproductive 3 3 a3 (Sum3 a1 a2 a3)  where 
    coproduct  = Sum33
    cofactor (Sum33 a) = Just a
    cofactor _         = Nothing

-- Coproduct 4
-------------------------------------------------------------------------------
data Sum4 a1 a2 a3 a4
    = Sum41 {-# UNPACK #-} !a1
    | Sum42 {-# UNPACK #-} !a2
    | Sum43 {-# UNPACK #-} !a3
    | Sum44 {-# UNPACK #-} !a4
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

type instance Coproduct (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4

instance Coproductive 4 1 a1 (Sum4 a1 a2 a3 a4)  where 
    coproduct  = Sum41
    cofactor (Sum41 a) = Just a
    cofactor _         = Nothing

instance Coproductive 4 2 a2 (Sum4 a1 a2 a3 a4)  where 
    coproduct  = Sum42
    cofactor (Sum42 a) = Just a
    cofactor _         = Nothing

instance Coproductive 4 3 a3 (Sum4 a1 a2 a3 a4)  where 
    coproduct  = Sum43
    cofactor (Sum43 a) = Just a
    cofactor _         = Nothing

instance Coproductive 4 4 a4 (Sum4 a1 a2 a3 a4)  where 
    coproduct  = Sum44
    cofactor (Sum44 a) = Just a
    cofactor _         = Nothing

-- Coproduct 5
-------------------------------------------------------------------------------
data Sum5 a1 a2 a3 a4 a5
    = Sum51 {-# UNPACK #-} !a1
    | Sum52 {-# UNPACK #-} !a2
    | Sum53 {-# UNPACK #-} !a3
    | Sum54 {-# UNPACK #-} !a4
    | Sum55 {-# UNPACK #-} !a5
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

type instance Coproduct (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5

instance Coproductive 5 1 a1 (Sum5 a1 a2 a3 a4 a5)  where 
    coproduct  = Sum51
    cofactor (Sum51 a) = Just a
    cofactor _         = Nothing

instance Coproductive 5 2 a2 (Sum5 a1 a2 a3 a4 a5)  where 
    coproduct  = Sum52
    cofactor (Sum52 a) = Just a
    cofactor _         = Nothing

instance Coproductive 5 3 a3 (Sum5 a1 a2 a3 a4 a5)  where 
    coproduct  = Sum53
    cofactor (Sum53 a) = Just a
    cofactor _         = Nothing

instance Coproductive 5 4 a4 (Sum5 a1 a2 a3 a4 a5)  where 
    coproduct  = Sum54
    cofactor (Sum54 a) = Just a
    cofactor _         = Nothing

instance Coproductive 5 5 a5 (Sum5 a1 a2 a3 a4 a5)  where 
    coproduct  = Sum55
    cofactor (Sum55 a) = Just a
    cofactor _         = Nothing

-- Coproduct 6
-------------------------------------------------------------------------------
data Sum6 a1 a2 a3 a4 a5 a6
    = Sum61 {-# UNPACK #-} !a1
    | Sum62 {-# UNPACK #-} !a2
    | Sum63 {-# UNPACK #-} !a3
    | Sum64 {-# UNPACK #-} !a4
    | Sum65 {-# UNPACK #-} !a5
    | Sum66 {-# UNPACK #-} !a6
    deriving (Eq, Ord, Data, Generic, Typeable, Show)

type instance Coproduct (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6

instance Coproductive 6 1 a1 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum61
    cofactor (Sum61 a) = Just a
    cofactor _         = Nothing

instance Coproductive 6 2 a2 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum62
    cofactor (Sum62 a) = Just a
    cofactor _         = Nothing

instance Coproductive 6 3 a3 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum63
    cofactor (Sum63 a) = Just a
    cofactor _         = Nothing

instance Coproductive 6 4 a4 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum64
    cofactor (Sum64 a) = Just a
    cofactor _         = Nothing

instance Coproductive 6 5 a5 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum65
    cofactor (Sum65 a) = Just a
    cofactor _         = Nothing

instance Coproductive 6 6 a6 (Sum6 a1 a2 a3 a4 a5 a6)  where 
    coproduct  = Sum66
    cofactor (Sum66 a) = Just a
    cofactor _         = Nothing

-- Coproduct 7
-------------------------------------------------------------------------------
data Sum7 a1 a2 a3 a4 a5 a6 a7
    = Sum71 {-# UNPACK #-} !a1
    | Sum72 {-# UNPACK #-} !a2
    | Sum73 {-# UNPACK #-} !a3
    | Sum74 {-# UNPACK #-} !a4
    | Sum75 {-# UNPACK #-} !a5
    | Sum76 {-# UNPACK #-} !a6
    | Sum77 {-# UNPACK #-} !a7
    deriving (Eq, Ord, Data, Generic, Typeable, Functor, Show)


type instance Coproduct (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7

instance Coproductive 7 1 a1 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum71
    cofactor (Sum71 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 2 a2 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum72
    cofactor (Sum72 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 3 a3 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum73
    cofactor (Sum73 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 4 a4 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum74
    cofactor (Sum74 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 5 a5 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum75
    cofactor (Sum75 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 6 a6 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum76
    cofactor (Sum76 a) = Just a
    cofactor _         = Nothing

instance Coproductive 7 7 a7 (Sum7 a1 a2 a3 a4 a5 a6 a7)  where 
    coproduct  = Sum77
    cofactor (Sum77 a) = Just a
    cofactor _         = Nothing
    