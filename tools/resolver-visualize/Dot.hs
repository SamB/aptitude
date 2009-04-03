-- | Haskell code to output files for graphviz's "dot".
--
-- This is a thin wrapper; it doesn't attempt to typecheck things like
-- attribute values.

module Dot(
           Digraph, Node, Edge,
           AttributeValue, Attributed,
           Name(), AttrValue(),
           name, attrValue,
           node, edge, digraph,
           genNodes,
           (.=), (..=), (.!)
          )
           where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

newtype Name = Name String deriving(Eq, Ord)
newtype AttrValue = AttrValue String deriving(Eq, Ord)

name :: String -> Name
name = Name

attrValue :: String -> AttrValue
attrValue = AttrValue

instance Show Name where
    showsPrec _ (Name n) = shows n

instance Show AttrValue where
    showsPrec _ (AttrValue v) = shows v

intersperse = List.intersperse
foldl' = List.foldl'

class AttributeValue v where
    valueString :: v -> AttrValue

instance AttributeValue Integer where
    valueString = attrValue . show

instance AttributeValue Int where
    valueString = attrValue . show

instance AttributeValue AttrValue where
    valueString = id

instance AttributeValue BS.ByteString where
    valueString = attrValue . BS.unpack



-- Nothing represents an attribute with no value, like "decorate".
type Attributes = Map.Map Name (Maybe AttrValue)

class Attributed a where
    addAttribute :: a -> Name -> (Maybe AttrValue) -> a

(.=) :: (Attributed a, AttributeValue v) => a -> (String, v) -> a
a .= (nameString, v) = addAttribute a (name nameString) (Just $ valueString v)

-- EW: hack to pretend-overload .= on strings.
(..=) :: (Attributed a) => a -> (String, String) -> a
a ..= (nameString, valString) = addAttribute a (name nameString) (Just $ attrValue valString)

(.!) :: Attributed a => a -> Name -> a
a .! name = addAttribute a name Nothing


data Digraph = Digraph { digraphNodes :: [Node],
                         digraphEdges :: [Edge],
                         digraphAttributes :: Attributes }

data Node = Node { nodeName :: Name,
                   nodeAttributes :: Attributes }

data Edge = Edge { edgeFrom :: Node,
                   edgeTo   :: Node,
                   edgeAttributes :: Attributes }

node :: Name -> Node
node name = Node { nodeName = name,
                   nodeAttributes = Map.empty }

edge :: Node -> Node -> Edge
edge from to = Edge { edgeFrom = from,
                      edgeTo = to,
                      edgeAttributes = Map.empty }

digraph :: [Node] -> [Edge] -> Digraph
digraph nodes edges = Digraph { digraphNodes = nodes,
                                digraphEdges = edges,
                                digraphAttributes = Map.empty }

-- | An infinite list of nodes with arbitrary names.
genNodes = [node (name $ "node" ++ show n) | n <- [1..]]


instance Attributed Digraph where
    addAttribute dg name value = dg { digraphAttributes = Map.insert name value (digraphAttributes dg) }

instance Attributed Node where
    addAttribute n name value = n { nodeAttributes = Map.insert name value (nodeAttributes n) }

instance Attributed Edge where
    addAttribute e name value = e { edgeAttributes = Map.insert name value (edgeAttributes e) }


showsAttribute :: Name -> Maybe AttrValue -> ShowS
showsAttribute (Name name) Nothing = shows name
showsAttribute (Name name) (Just (AttrValue value)) =
    shows name . ('=':) . shows value

showsAttributes :: Attributes -> ShowS
showsAttributes as = if Map.null as
                     then id
                     else let listEntries =
                                  (intersperse (", "++)
                                   [showsAttribute name value
                                        | (name, value) <- Map.toList as]) in
                          (" ["++) . foldl' (.) id listEntries . (']':)

instance Show Node where
    showsPrec _ n = shows (nodeName n) . showsAttributes (nodeAttributes n)

instance Show Edge where
    showsPrec _ e = shows (nodeName $ edgeFrom e) . (" -> "++) .
                    shows (nodeName $ edgeTo e) . showsAttributes (edgeAttributes e)

instance Show Digraph where
    showsPrec _ dg = let nodes = map (.(";\n"++)) [("  "++) . shows n
                                                       | n <- digraphNodes dg]
                         edges = map (.(";\n"++)) [("  "++) . shows e
                                                       | e <- digraphEdges dg]
                         attrs = map (.(";\n"++)) [("  "++) . showsAttribute k v
                                                       | (k, v) <- Map.toList $ digraphAttributes dg]
                         lines = foldl' (.) id $
                                 intersperse ("\n"++) (foldNonEmpty [attrs, nodes, edges])
                     in
                       ("digraph {\n"++) . lines . ("}"++)
        where foldNonEmpty lst = [foldl' (.) id x | x <- lst, not (null x)]
