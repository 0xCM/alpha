-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Grid
(
    module X,
) where
import Alpha.Canonical
import Alpha.Linear.Matrix as X


instance Cellular (MatrixCell v) where
    type Location (MatrixCell v) = (Int,Int)
    type Value (MatrixCell v) = v

    cell loc val = Cell(loc,val)    
    location (Cell (x,_)) = x
    value (Cell (_,a)) = a
        
instance (Formattable v) => Formattable (MatrixCell v) where
    format (Cell ((i,j),a)) = format (i,j,a)
    
instance (Formattable v) => Show (MatrixCell v) where
    show = string . format    
