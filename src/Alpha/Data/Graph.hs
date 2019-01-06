module Alpha.Data.Graph
(
    Vertex(..),
    Edge(..),
    Graph(..),
    edge, vertex, graph,
    vertices, edges
) where
import Alpha.Canonical
import qualified Data.List as List

-- | Represents a labeled graph node
data Vertex s a = Vertex s a
    deriving(Eq,Ord,Data,Typeable,Generic)

type instance Individual (Vertex s a) = a

-- | Represents a directed, labled edge in a graph
data Edge s a = Edge s (Vertex s a) (Vertex s a)
    deriving(Eq,Ord,Data,Typeable,Generic)

type instance Individual (Edge s a) = Vertex s a
    
-- | Represents a directed grap
newtype Graph s a = Graph [Edge s a]
    deriving(Eq,Ord,Data,Typeable,Generic)
instance Newtype(Graph s a)    

type instance Individual (Graph s a) = Edge s a        


-- | Defines a labeled edge
edge::(Ord a, SemiOrd s) => s -> Vertex s a -> Vertex s a -> Edge s a
edge label src dst = Edge label src dst

-- | Defines a labeled node
vertex::(Ord a, SemiOrd s) => s -> a -> Vertex s a
vertex = Vertex

-- | Constructs a graph defined by a set of edges
graph::(Ord a, SemiOrd s) => [Edge s a] -> Graph s a
graph = Graph

-- | Returns a graph's vertexes
vertices::(Ord a, SemiOrd s) => Graph s a -> [Vertex s a]
vertices (Graph edges) = result where
    result = edges |> toList |> fmap (\(Edge _ v1 v2) -> [v1,v2] ) |> unions

-- | Returns a graph's edges
edges::(Ord a, SemiOrd s) => Graph s a -> [Edge s a]
edges = unwrap

instance (Ord a, SemiOrd s) => Concatenable (Vertex s a) (Vertex s a)  where    
    type Concatenated (Vertex s a) (Vertex s a) = Edge s a
    concat src dst = edge lbl src dst  where
        lbl = (getLabel src) <> (getLabel dst)

instance (Formattable s, Formattable a) => Formattable (Vertex s a) where
    format (Vertex s a) = parenthetical (format s <> spaced Colon <> format a)        

instance (Formattable s, Formattable a) => Show (Vertex s a) where
    show = string . format

instance (Formattable s, Formattable a) => Formattable (Edge s a) where
    format (Edge s source target) 
        = format s <> lspaced Colon <> format source <> spaced FSlash <> format target

instance (Formattable s, Formattable a) => Show (Edge s a) where
    show = string . format

instance (Formattable s, Formattable a) => Formattable (Graph s a) where
    format (Graph edges) 
        = format <$> edges |> List.intersperse EOL |> append
        
instance (Formattable s, Formattable a) => Show (Graph s a) where
    show = string . format

instance Sourced (Edge s a) where
    type Source (Edge s a) = Vertex s a
    source (Edge _ source _) = source

instance Targeted (Edge s a) where
    type Target (Edge s a) = Vertex s a
    target (Edge _ _ target) = target
    
instance Labeled (Edge s a) where
    type Label (Edge s a) = s
    label s (Edge _ source target) = Edge s source target
    getLabel (Edge l _ _) = l

instance Labeled (Vertex s a) where
    type Label (Vertex s a) = s
    label s (Vertex _ a) = Vertex s a
    getLabel (Vertex s _) = s

instance Componentized (Graph s a) where
    components (Graph g) = g

instance Componentized (Edge s a) where
    components (Edge _ s t) = [s,t]
    
instance Functor (Vertex s) where
    fmap f (Vertex s a) = Vertex s (f a)
    
instance Functor (Edge s) where
    fmap f (Edge s source target) = Edge s (f <$> source) (f <$> target)
    
instance Functor (Graph s) where
    fmap f (Graph edges) = Graph [f <$> e | e <- edges]

instance Queryable (Graph s a) where
    filter pred (Graph edges) = List.filter pred edges
    
instance Queryable (Edge s a) where
    filter pred (Edge label source target) 
        = List.filter pred [source, target]
            