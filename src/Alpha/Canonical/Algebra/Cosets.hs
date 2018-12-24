-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Cosets
(
    Coset(..), 
    leftCoset,
    rightCoset
) where
import Alpha.Canonical.Relations


data Coset a
    = LeftCoset a [a] [a]
    | RightCoset a [a] [a]

leftCoset::(BinaryOperator f a) => f -> a -> [a] -> Coset a
leftCoset f h g = (*) <$> g |> LeftCoset h g
    where (*) = (o2 f) h

rightCoset::(BinaryOperator f a) => f -> [a] -> a -> Coset a
rightCoset f g h = (*) <$> g |> RightCoset h g
    where (*) = (\x -> (o2 f) x h)
