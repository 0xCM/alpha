module Alpha.Examples.Vectors where
import Alpha


instance Example "v-1" where
    example = do
        let v1 = vecN @3 [1::Int,2,3]
        let v2 = vecN @3 [4,5,6]
        let p = v1 .*. v2
        print p

