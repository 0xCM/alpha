module Alpha.Data.Graph
(
    Vertex(..),
    Edge(..),
    CrossEdge(..),
    Graph(..),
    edge,
    xedge,
    vertex, 
    graph,
    vertices, 
    edges
) where
import Alpha.Canonical
import qualified Data.List as List

-- | Represents a labeled graph node
data Vertex l a = Vertex l a
    deriving(Eq,Ord,Data,Typeable,Generic)

type instance Individual (Vertex l a) = a

-- | Represents a directed, labled edge in a graph
data Edge l a = Edge l (Vertex l a) (Vertex l a)
    deriving(Eq,Ord,Data,Typeable,Generic)

type instance Individual (Edge l a) = Vertex l a

-- | Represents a directed, labeled edge that connects
-- an node in one graph to a node in another graph
data CrossEdge l a b = CrossEdge l (Vertex l a) (Vertex l b)
    deriving(Eq,Ord,Data,Typeable,Generic)
    
-- | Represents a directed grap
newtype Graph l a = Graph [Edge l a]
    deriving(Eq,Ord,Data,Typeable,Generic)
instance Newtype(Graph l a)    

type instance Individual (Graph s a) = Edge s a        


-- | Defines a labeled edge
edge::(Ord a, SemiOrd s) => s -> Vertex s a -> Vertex s a -> Edge s a
edge label src dst = Edge label src dst

xedge::(SemiOrd l, Ord a, Ord b) => l -> Vertex l a -> Vertex l b -> CrossEdge l a b
xedge label src dst = CrossEdge label src dst

-- | Defines a labeled node
vertex::(SemiOrd l, Ord a) => l -> a -> Vertex l a
vertex = Vertex

-- | Constructs a graph defined by a set of edges
graph::(SemiOrd l, Ord a) => [Edge l a] -> Graph l a
graph = Graph

-- | Returns a graph's vertexes
vertices::(Ord a, SemiOrd s) => Graph s a -> [Vertex s a]
vertices (Graph edges) = result where
    result = edges |> toList |> fmap (\(Edge _ v1 v2) -> [v1,v2] ) |> unions

-- | Returns a graph's edges
edges::(Ord a, SemiOrd s) => Graph s a -> [Edge s a]
edges = unwrap

instance (SemiOrd l, Ord a, Ord b) => Connective l (Vertex l a) (Vertex l b) where
    type Connection l (Vertex l a) (Vertex l b) = CrossEdge l a b
    connect label a b= xedge label a b            

instance (Ord a, SemiOrd l) => Concatenable (Vertex l a) (Vertex l a)  where    
    type Concatenated (Vertex l a) (Vertex l a) = Edge l a
    concat src dst = edge lbl src dst  where
        lbl = (getLabel src) <> (getLabel dst)

instance (Formattable l, Formattable a) => Formattable (Vertex l a) where
    format (Vertex l a) = parenthetical (format l <> spaced Colon <> format a)        

instance (Formattable s, Formattable a) => Show (Vertex s a) where
    show = string . format

instance Functor (Vertex s) where
    fmap f (Vertex s a) = Vertex s (f a)
    
instance Labeled (Vertex l a) l where
    label l (Vertex _ a) = Vertex l a
    getLabel (Vertex l _) = l
        
instance (Formattable s, Formattable a) => Formattable (Edge s a) where
    format (Edge s source target) 
        = format s <> lspaced Colon <> format source <> spaced FSlash <> format target

instance (Formattable l, Formattable a) => Show (Edge l a) where
    show = string . format
        
instance Sourced (Edge s a) where
    type Source (Edge s a) = Vertex s a
    source (Edge _ source _) = source

instance Targeted (Edge l a) where
    type Target (Edge l a) = Vertex l a
    target (Edge _ _ target) = target
        
instance Labeled (Edge l a) l where
    label s (Edge _ source target) = Edge s source target
    getLabel (Edge l _ _) = l
    
instance Componentized (Edge s a) where
    components (Edge _ s t) = [s,t]
        
instance Functor (Edge s) where
    fmap f (Edge s source target) = Edge s (f <$> source) (f <$> target)

instance Componentized (Graph s a) where
    components (Graph g) = g
        
instance Functor (Graph s) where
    fmap f (Graph edges) = Graph [f <$> e | e <- edges]

instance Queryable (Graph s a) where
    filter pred (Graph edges) = List.filter pred edges
    
instance Queryable (Edge s a) where
    filter pred (Edge label source target) 
        = List.filter pred [source, target]
        
instance (Formattable s, Formattable a) => Formattable (Graph s a) where
    format (Graph edges) 
        = format <$> edges |> List.intersperse EOL |> append
        
instance (Formattable s, Formattable a) => Show (Graph s a) where
    show = string . format
        
instance Bifunctor (CrossEdge s) where
    bimap f1 f2 (CrossEdge l source target) = CrossEdge l (f1 <$> source) (f2 <$> target)
                
instance Sourced (CrossEdge l a b) where
    type Source (CrossEdge l a b) = Vertex l a
    source (CrossEdge _ source _) = source

instance Targeted (CrossEdge l a b) where
    type Target (CrossEdge l a b) = Vertex l b
    target (CrossEdge _ _ target) = target

instance Labeled (CrossEdge l a b) l where
    label l (CrossEdge _ source target) = CrossEdge l source target
    getLabel (CrossEdge l _ _) = l
        
instance (Formattable l, Formattable a, Formattable b) => Formattable (CrossEdge l a b) where
    format (CrossEdge l source target) 
        = format l <> lspaced Colon <> format source <> spaced FSlash <> format target

instance (Formattable l, Formattable a, Formattable b) => Show (CrossEdge l a b) where
    show = string . format
    
        