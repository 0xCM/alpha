module Alpha.Data.Numbers.Test where

import Alpha
import Alpha.Claim

concatT::IO()
concatT = do

    let x = word8 12
    let y = word8 4
    let z = x +++ y

    print z
