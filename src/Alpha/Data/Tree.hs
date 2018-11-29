module Alpha.Data.Tree
(
    Tree, 
    Forest,
    testTree
)

where
import Alpha.Base
import Alpha.Canonical
import Data.Tree

instance Collapsible (Tree a) where
    type Collapsed (Tree a) = [a]
    collapse = flatten

tree::(b -> (a, [b])) -> b-> Tree a
tree = unfoldTree


testTree = tree (\x -> (x, [1..x])) 5
