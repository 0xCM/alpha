-----------------------------------------------------------------------------
-- | Defines coproduct machinery
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Coproduct
(
    Coproduct(..), Coproductive(..)
    
)
where
import Alpha.Base
import Alpha.Data.Product
import Alpha.Data.Sum
import Alpha.Data.Func

type family Coproduct a = r | r -> a where
    Coproduct (Sum1 a1) = Sum1 a1
    Coproduct (Sum2 a1 a2) = Sum2 a1 a2     
    Coproduct (Sum3 a1 a2 a3) = Sum3 a1 a2 a3
    Coproduct (Sum4 a1 a2 a3 a4) = Sum4 a1 a2 a3 a4
    Coproduct (Sum5 a1 a2 a3 a4 a5) = Sum5 a1 a2 a3 a4 a5
    Coproduct (Sum6 a1 a2 a3 a4 a5 a6) = Sum6 a1 a2 a3 a4 a5 a6
    Coproduct (Sum7 a1 a2 a3 a4 a5 a6 a7) = Sum7 a1 a2 a3 a4 a5 a6 a7
    Coproduct (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) = Sum8 a1 a2 a3 a4 a5 a6 a7 a8

class Coproductive (m::Nat) (n::Nat) a b where
    -- | Injects an a-value into a b-valued coproduct
    coproduct::a -> Coproduct b

    -- | Selects an a-value from a b-valued coproduct if it exists
    cofactor::Coproduct b -> Maybe a

-- Coproduct 1         
instance Coproductive 1 1 a1 (Sum1 a1 )  where 
    coproduct  = Sum11    
    cofactor (Sum11 a) = Just a

-- Coproduct 2     
-------------------------------------------------------------------------------

instance Coproductive 2 1 a1 (Sum2 a1 a2)  where 
    coproduct  = Sum21    
    cofactor (Sum21 a) = Just a
    cofactor _         = Nothing

instance Coproductive 2 2 a2 (Sum2 a1 a2)  where 
    coproduct  = Sum22
    cofactor (Sum22 a) = Just a
    cofactor _         = Nothing

type CoFunc2 f1 a1 b1 f2 a2 b2
    = (f1 ~ Func1 a1 b1, f2 ~ Func1 a2 b2)
    
comap2::CoFunc2 f1 a1 b1 f2 a2 b2 
    => Product2 f1 f2 -> (Sum2 a1 a2) -> (Sum2 b1 b2)
comap2 (Product2 f1 _ ) (Sum21 a1) = Sum21 (f1 a1)
comap2 (Product2 _ f2 ) (Sum22 a2) = Sum22 (f2 a2)  
    
-- Coproduct 3
-------------------------------------------------------------------------------

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

type CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3
    = (CoFunc2 f1 a1 b1 f2 a2 b2, f3 ~ Func1 a3 b3)

comap3::CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3 
    => Product3 f1 f2 f3 -> (Sum3 a1 a2 a3) -> (Sum3 b1 b2 b3)
comap3 (Product3 f1 _ _) (Sum31 a1) = Sum31 (f1 a1)
comap3 (Product3 _ f2 _) (Sum32 a2) = Sum32 (f2 a2)  
comap3 (Product3 _ _ f3) (Sum33 a3) = Sum33 (f3 a3)
    
-- Coproduct 4
-------------------------------------------------------------------------------

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

type CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4  
    = (CoFunc3 f1 a1 b1 f2 a2 b2 f3 a3 b3, f4 ~ Func1 a4 b4)

comap4::(CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4) 
    => Product4 f1 f2 f3 f4 -> (Sum4 a1 a2 a3 a4) -> (Sum4 b1 b2 b3 b4)
comap4 (Product4 f1 _ _ _) (Sum41 a1) = Sum41 (f1 a1)
comap4 (Product4 _ f2 _ _) (Sum42 a2) = Sum42 (f2 a2)  
comap4 (Product4 _ _ f3 _) (Sum43 a3) = Sum43 (f3 a3)
comap4 (Product4 _ _ _ f4) (Sum44 a4) = Sum44 (f4 a4)    

-- Coproduct 5
-------------------------------------------------------------------------------

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

type CoFunc5 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5 
    = (CoFunc4 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4, f5 ~ Func1 a5 b5)

comap5::CoFunc5 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5 
    => Product5 f1 f2 f3 f4 f5 -> (Sum5 a1 a2 a3 a4 a5) -> (Sum5 b1 b2 b3 b4 b5)
comap5 (Product5 f1 _ _ _ _) (Sum51 a1) = Sum51 (f1 a1)
comap5 (Product5 _ f2 _ _ _) (Sum52 a2) = Sum52 (f2 a2)  
comap5 (Product5 _ _ f3 _ _) (Sum53 a3) = Sum53 (f3 a3)
comap5 (Product5 _ _ _ f4 _) (Sum54 a4) = Sum54 (f4 a4)    
comap5 (Product5 _ _ _ _ f5) (Sum55 a5) = Sum55 (f5 a5)    

-- Coproduct 6
-------------------------------------------------------------------------------

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

type CoFunc6 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5 f6 a6 b6
    = (CoFunc5 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5, f6 ~ Func1 a6 b6)

comap6::CoFunc6 f1 a1 b1 f2 a2 b2 f3 a3 b3 f4 a4 b4 f5 a5 b5 f6 a6 b6
    => Product6 f1 f2 f3 f4 f5 f6 -> (Sum6 a1 a2 a3 a4 a5 a6) -> (Sum6 b1 b2 b3 b4 b5 b6)
comap6 (Product6 f1 _ _ _ _ _) (Sum61 a1) = Sum61 (f1 a1)
comap6 (Product6 _ f2 _ _ _ _) (Sum62 a2) = Sum62 (f2 a2)  
comap6 (Product6 _ _ f3 _ _ _) (Sum63 a3) = Sum63 (f3 a3)
comap6 (Product6 _ _ _ f4 _ _) (Sum64 a4) = Sum64 (f4 a4)    
comap6 (Product6 _ _ _ _ f5 _) (Sum65 a5) = Sum65 (f5 a5)    
comap6 (Product6 _ _ _ _ _ f6) (Sum66 a6) = Sum66 (f6 a6)    
-- Coproduct 7
-------------------------------------------------------------------------------

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


-- Coproduct 8
-------------------------------------------------------------------------------

instance Coproductive 8 1 a1 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum81
    cofactor (Sum81 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 2 a2 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum82
    cofactor (Sum82 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 3 a3 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum83
    cofactor (Sum83 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 4 a4 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum84
    cofactor (Sum84 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 5 a5 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum85
    cofactor (Sum85 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 6 a6 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum86
    cofactor (Sum86 a) = Just a
    cofactor _         = Nothing

instance Coproductive 8 7 a7 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8) where 
    coproduct  = Sum87
    cofactor (Sum87 a) = Just a
    cofactor _         = Nothing
    
instance Coproductive 8 8 a8 (Sum8 a1 a2 a3 a4 a5 a6 a7 a8)  where 
    coproduct  = Sum88
    cofactor (Sum88 a) = Just a
    cofactor _         = Nothing
        
