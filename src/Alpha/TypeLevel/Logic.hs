-----------------------------------------------------------------------------
-- | Basic type-level support
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Alpha.TypeLevel.Logic where

import Alpha.Base
import Data.Kind(Type)  
import Data.Proxy
import qualified GHC.TypeLits as TL
import qualified GHC.Natural as TN
import GHC.Types(Symbol)
import Data.Type.Equality hiding (type (==), apply)
    
type True = 'True
type False = 'False

type family Equal a b :: Bool where
    Equal a a = True
    Equal a b = False    

type family Or (a::Bool) (b::Bool) :: Bool where
    Or True True = True
    Or True False = True
    Or False True = True
    Or False False = False

type a :||: b = Or a b

or'::Proxy a -> Proxy b -> Proxy (a :||: b)
or' Proxy Proxy = Proxy
    

type family And (a::Bool) (b::Bool) :: Bool where
    And True True = True
    And True False = False
    And False True = False
    And False False = False    

type family Not (a::Bool) :: Bool where    
    Not True = False
    Not False = True

type family If (a::Bool) (b::k) (c::k) :: k where
    If True b c = b
    If False b c = c    

-- | A label to denote the concept of "true"
data T = T
    deriving(Show)

-- | A label to denote the concept of "false"
data F = F
    deriving(Show)
    
-- | Logical False
type family LFalse a where
    LFalse T = F
    LFalse F = F
    
-- | Logical True    
type family LTrue a where
    LTrue T = T
    LTrue F = F

-- | Logical Negation
type family (:~) a where
    (:~) T = F
    (:~) F = T

-- | Logical And    
type family (:&) a b where
    (:&) F F = F
    (:&) F T = F
    (:&) T F = F
    (:&) T T = T

-- | Logical Or        
type family (:|) a b where
    (:|) F F = F
    (:|) F T = T
    (:|) T F = T
    (:|) T T = T

-- | Logical XOr        
type family (:^) a b where
    (:^) F F = F
    (:^) F T = T
    (:^) T F = T
    (:^) T T = F
    
-- | Implies    
type family (:=>) a b where
    (:=>) F F = T
    (:=>) F T = T
    (:=>) T F = F
    (:=>) T T = T    
