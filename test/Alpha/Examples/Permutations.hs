{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Examples.Permutations where
import Alpha

default(Int,Double,Text) 

instance Example "p-1" where
    example = do
        let idperm = permutation @3 [(i,i) | i <- [1..10]]
        print $ format  idperm
    
-- instance Example "p-2" where
--     example = do
--         let p1 = perm @3 [(1,2), (2,3), (3,1)]
--         print $ "p1 = " <> (show p1)
--         let p2 = invert p1
--         print $ "invert p1 = " <> (show p2)
--         let p3 = p1 <> p2 
--         print $ "p1 . p2 = " <> (show p3)
        
instance Example "p-3" where
    example = do
        let s3 = symgroup @3
        print (format s3)

instance Example "p-4" where
    example = do
        let s4 = symgroup @4
        let p2 = s4 !! 2
        print p2
        let p5 = s4 !! 5        
        print p5
        let p = p2 <> p5
        print p
        --let pI = invert p

instance Example "p-5" where
    example = do
        let s3 = symgroup @3
        let p2 = s3 !! 2
        print p2

        let p3 = switch (1,2) p2
        print p3

        let p4 = switch (1,2) p3
        print p4

        print <| ifelse (p2 == p4) ":)))" ":~~("
            