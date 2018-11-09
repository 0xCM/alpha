{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Disjoint where

import Alpha.Base

-- Defines an indexed family of disjoint unions
type family Disjoint (n::Nat) a where
    Disjoint 1 a = [a]
    Disjoint 2 (x1, x2) = ([x1], [x2])
    Disjoint 3 (x1, x2, x3) = ([x1], [x2], [x3])
    Disjoint 4 (x1, x2, x3, x4) = ([x1], [x2], [x3], [x4])
    Disjoint 5 (x1, x2, x3, x4, x5) = ([x1], [x2], [x3], [x4], [x5])

type x1 \./ x2 = Disjoint 2 (x1, x2)

class Injector (n::Nat) (i::Nat) a b where
    inject::a -> Disjoint n b

instance Injector 1 1 x1 x1 where
    inject x = [x]
    
instance Injector 2 1 x1 (x1,x2) where
    inject x = ([x], [])

instance Injector 2 2 x2 (x1,x2) where
    inject x = ([], [x])
        
instance Injector 3 1 x1 (x1,x2,x3) where
    inject x = ([x], [], [])

instance Injector 3 2 x2 (x1,x2,x3) where
    inject x = ([], [x], [])

instance Injector 3 3 x3 (x1,x2,x3) where
    inject x = ([], [], [x])
    