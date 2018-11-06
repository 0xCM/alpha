module Alpha.Data.Function where

import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product

data Func f = 
    forall x1         
        . Func1 x1

    | forall x1 x2        
        . Func2 (x1->x2)

    | forall x1 x2 x3      
        . Func3 (x1 -> x2 -> x3)

    | forall x1 x2 x3 x4
        . Func4 (x1 -> x2 -> x3 -> x4)

    | forall x1 x2 x3 x4 x5
        . Func5 (x1 -> x2 -> x3 -> x4 -> x5)

    | forall x1 x2 x3 x4 x5 x6 
        . Func6 (x1 -> x2 -> x3 -> x4 -> x5 -> x6)


func1::x -> Func x
func1 = Func1

func2::(x1->x2) -> Func (x1->x2)
func2 = Func2

func3::(x1 -> x2 -> x3) -> Func(x1 -> x2 -> x3)
func3 = Func3

func4::(x1 -> x2 -> x3 -> x4) -> Func(x1 -> x2 -> x3 -> x4)
func4 = Func4

func5::(x1 -> x2 -> x3 -> x4 -> x5) -> Func(x1 -> x2 -> x3 -> x4 -> x5)
func5 = Func5

func6::(x1 -> x2 -> x3 -> x4 -> x5 -> x6) -> Func(x1 -> x2 -> x3 -> x4 -> x5 -> x6)
func6 = Func6

