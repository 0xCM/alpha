module Alpha.Canonical.Structures.Simplex
(
    Simplex(..),
    SimplicalComplex(..)
)
where
import Alpha.Canonical.Algebra
-- This development foolows the interpretation of a simplex as defined by Y2008DAT
-- which states on p. 197 that:
-- A simplicial complex K := (E,S) consists of a set 
-- E of vertices and a set S of finite non-empty subsets of E. 
-- An element s ∊ S with q + 1 elements is called a *q-simplex* 
-- of K provided the following axioms hold:
-- (1) The singleton {e} ∊ S for each e ∊ E.
-- (2) If t ∊ S and s ⊂ t is non-empty, then s ∊ S
-- If s ∊ S is a q-simplex and t ⊂ s, then t is called a *face*
-- of s

newtype Simplex a = Simplex (Natural, NonEmpty a)
    deriving (Eq,Ord,Generic)

newtype SimplicalComplex e = SimplicalComplex ([e], [Simplex e])

instance Dimensional (Simplex e) where
    type Dimension (Simplex e) = Natural
    dimension (Simplex (q,_)) = integral q

-- | Extracts the simplexes form the complex of dimension 'q'    
simplices::(Ord a) => Natural -> SimplicalComplex a -> [Simplex a]
simplices q (SimplicalComplex (edges, sim)) = sim |> filter (\s -> dimension s == q)

edges::SimplicalComplex a -> [a]
edges (SimplicalComplex  (e, _) ) = e


-- | Constructs a simplex from a nonempty list
simplex::NonEmpty a -> Simplex a
simplex (h :| t)  = Simplex( (length l) -1, l) 
    where l = h :| t

-- | Constructs a vertex from a single point
vertex::a -> Simplex a
vertex a = Simplex (0, a :| [])

-- | Constructs a vertex for a list of points
vertices::[a] -> [Simplex a]
vertices points = vertex <$> points
