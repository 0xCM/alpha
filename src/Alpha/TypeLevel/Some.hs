-----------------------------------------------------------------------------
-- | A type-level Some representation, derivative from Galois bv-sized and 
-- parameterized-utils libraries; see LICENSE
-- Copyright   :  (c) 0xCM, 2018
-- License     :  Per accompanying LICENSE file
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Alpha.TypeLevel.Some where
import Data.Hashable
import Data.Proxy as Proxy
import Data.Type.Equality as Equality
import Data.Traversable
import Data.Functor
import Data.Function
import Prelude(($!))
    
data Some (f:: k -> *) = forall x . Some (f x)

viewSome :: (forall tp . f tp -> r) -> Some f -> r
viewSome f (Some x) = f x

mapSome :: (forall tp . f tp -> g tp) -> Some f -> Some g
mapSome f (Some x) = Some $! f x

{-# INLINE traverseSome #-}
-- | Modify the inner value.
traverseSome :: Functor m
                => (forall tp . f tp -> m (g tp))
                -> Some f
                -> m (Some g)
traverseSome f (Some x) = Some `fmap` f x


-- | Modify the inner value.
traverseSome_ :: Functor m => (forall tp . f tp -> m ()) -> Some f -> m ()
traverseSome_ f (Some x) = (\_ -> ()) `fmap` f x
{-# INLINE traverseSome_ #-}    
