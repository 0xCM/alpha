-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Data.Graph
(
    Vertex(..),
    Vertexed(..),
    Edge(..),
    Edged(..),
    Graph(..),
    Graphic(..),
    Crossing(..),
    CrossGraphic(..),    
) where
import Alpha.Canonical
import qualified Data.List as List

-- | Represents a labeled graph node
newtype Vertex l a = Vertex (l, a)
    deriving(Eq,Ord,Data,Typeable,Generic)
instance Newtype(Vertex l a)    

-- | Represents a directed, labled edge in a graph
data Edge l a = Edge l (Vertex l a) (Vertex l a)
    deriving(Eq,Ord,Data,Typeable,Generic)

-- | Represents a directed grap
newtype Graph l a = Graph [Edge l a]
    deriving(Eq,Ord,Data,Typeable,Generic)
instance Newtype(Graph l a)    

-- | Represents a path in a graph, i. e., an ordered sequence of
-- edges
newtype GraphPath l a = GraphPath [Edge l a]    
    deriving(Eq,Ord,Data,Typeable,Generic,Initializing,Terminating)
instance Newtype(GraphPath l a)    

type instance Individual (Vertex l a) = a
type instance Individual (Edge l a) = Vertex l a
type instance Individual (Graph l a) = Edge l a        
type instance Individual (GraphPath l a) = Edge l a 

-- | Characterizes a type that can be described by a vertex in a graph
class (SemiOrd l, Ord a) => Vertexed l a n where

    -- | Constructs a labled 'Vertex'
    vertex::l -> a -> n
    
-- | Characterizes a type that can be described by an edge in a graph
class (SemiOrd l, Ord n) => Edged l n e where

    -- | Constructs a labled 'Edge'
    edge::l -> n -> n -> e
    
-- | Characterizes a type that can be described by a collection of edges
-- that constitute a graph
class Graphic g e n where  

    -- Constructs a graph from a set of edges
    graph::[e] -> g

    -- Retrives the edges that comprise the graph
    edges::g -> [e]

    -- Retrieves the nodes contained in the graph
    nodes::g -> [n]

-- | Characterizes a type that supports cross-graph associations    
class (SemiOrd l, Ord a, Ord b) => CrossGraphic l a b where    
    xedge::l -> Vertex l a -> Vertex l b -> Crossing l a b
    xedge label src dst = Crossing label src dst
            
-- | Represents a directed, labeled edge that connects
-- an node in one graph to a node in another graph
data Crossing l a b = Crossing l (Vertex l a) (Vertex l b)
    deriving(Eq,Ord,Data,Typeable,Generic)

instance (SemiOrd l, Ord a) => Graphic (Graph l a) (Edge l a) (Vertex l a) where
    edges = unwrap
    graph = wrap 

    nodes (Graph edges) = edges  |> fmap (\(Edge _ v1 v2) -> [v1,v2] ) |> unions

instance (SemiOrd l, Ord a) => Vertexed l a (Vertex l a) where
    vertex l a = Vertex (l, a)

instance (SemiOrd l, Ord a) => Edged l (Vertex l a) (Edge l a) where
    edge l v1 v2 = Edge l v1 v2
        
instance (CrossGraphic l a b) => Connective l (Vertex l a) (Vertex l b) where
    type Connection l (Vertex l a) (Vertex l b) = Crossing l a b
    connect label a b= xedge label a b            

instance (Formattable l, Formattable a) => Formattable (Vertex l a) where
    format (Vertex (l, a)) = parenthetical (format l <> pad Colon <> format a)        

instance (Formattable s, Formattable a) => Show (Vertex s a) where
    show = string . format

instance Functor (Vertex s) where
    fmap f (Vertex (s, a)) = Vertex (s, (f a))
    
instance Labeled (Vertex l a) l where
    label l (Vertex (_,a)) = Vertex (l, a)
    getLabel (Vertex (l, _)) = l
        
instance (Formattable s, Formattable a) => Formattable (Edge s a) where
    format (Edge s source target) 
        = format s <> lpad Colon <> format source <> pad FSlash <> format target

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
        
instance Bifunctor (Crossing s) where
    bimap f1 f2 (Crossing l source target) = Crossing l (f1 <$> source) (f2 <$> target)
                
instance Sourced (Crossing l a b) where
    type Source (Crossing l a b) = Vertex l a
    source (Crossing _ source _) = source

instance Targeted (Crossing l a b) where
    type Target (Crossing l a b) = Vertex l b
    target (Crossing _ _ target) = target

instance Labeled (Crossing l a b) l where
    label l (Crossing _ source target) = Crossing l source target
    getLabel (Crossing l _ _) = l
        
instance (Formattable l, Formattable a, Formattable b) => Formattable (Crossing l a b) where
    format (Crossing l source target) 
        = format l <> lpad Colon <> format source <> pad FSlash <> format target

instance (Formattable l, Formattable a, Formattable b) => Show (Crossing l a b) where
    show = string . format        