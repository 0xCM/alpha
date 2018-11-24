{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
module Alpha.Data.Tuple
(
    Tuple(..)
)
where

import Alpha.Base
import Alpha.Canonical

type family Tuple a = r | r -> a where
    Tuple (a1,a2)  = (a1,a2)
    Tuple (a1,a2,a3) = (a1,a2,a3)
    Tuple (a1,a2,a3,a4) = (a1,a2,a3,a4)
    Tuple (a1,a2,a3,a4,a5)  = (a1,a2,a3,a4,a5)
    Tuple (a1,a2,a3,a4,a5,a6)  = (a1,a2,a3,a4,a5,a6)
    Tuple (a1,a2,a3,a4,a5,a6,a7)  = (a1,a2,a3,a4,a5,a6,a7)
    Tuple (a1,a2,a3,a4,a5,a6,a7,a8)  = (a1,a2,a3,a4,a5,a6,a7,a8)
    Tuple (a1,a2,a3,a4,a5,a6,a7,a8,a9)  = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
