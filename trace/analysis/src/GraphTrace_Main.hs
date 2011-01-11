-- Tool to produce a graph (as a dot file) from a trace
--
-- INPUT LINE FORMAT: <count> <module>
--
-- Count is the number of self jumps in the module before the jump 
-- to the next module (on the following line).
module Main(main) where

import Control.Monad
import Data.List
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Read as T
import Language.Dot.Syntax
import Language.Dot.Pretty
import System.IO
import System.Environment
import System.Exit


--------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------
type Node = Text
type Edge = (Node, Node)
type EdgeCount = Integer
data TraceGraph = TraceGraph {edges :: Map Edge EdgeCount, nodes :: Set Node}
  deriving(Show)

singletonGraph :: Node -> TraceGraph
singletonGraph node = TraceGraph {edges = M.empty, nodes = S.singleton node}

--------------------------------------------------------------------------
-- Graph Construction
--------------------------------------------------------------------------
buildGraph :: [Text] -> TraceGraph
buildGraph input = addEdge (lastNode, endNode) 1 graph
  where
  (lastNode, graph) = foldl' build (startNode, singletonGraph startNode) input
  startNode = T.pack "Start"
  endNode   = T.pack "End"
  build (prevNode, g) line = 
    let (node, cnt) = parseLine line
        newGraph    = addEdge (prevNode, node) 1 $ addEdge (node, node) cnt g
    in (node, newGraph)
    
parseLine :: Text -> (Node, EdgeCount)
parseLine line = 
  case T.words line of
    [cnt, node] -> (node, readCnt cnt)
    _           -> error("Invalid line: " ++ (show line))
  where 
  readCnt :: Text -> Integer
  readCnt i = 
    case T.decimal i of 
      Left err   -> error("Error parsing edge count: " ++ (show i) ++ " " ++ err)
      Right (d,_)-> d

addEdge ::  Edge -> EdgeCount -> TraceGraph -> TraceGraph
addEdge e@(source, sink) count g = 
  TraceGraph { 
    edges = M.insertWith (+) e count (edges g),
    nodes = S.insert source $ S.insert sink $ (nodes g) 
  }

--------------------------------------------------------------------------
-- Conversion to Dot
--------------------------------------------------------------------------
type DotGraph = Graph
convertToDot :: TraceGraph -> DotGraph
convertToDot graph = 
  Graph UnstrictGraph DirectedGraph Nothing statements
  where
  statements = nodeStatements ++ edgeStatements
  nodeStatements = map (\n -> NodeStatement (mkId n) []) ns
  edgeStatements = map (\((n1,n2),cnt) -> mkEdgeStmt n1 n2 cnt) es
  mkEdgeStmt n1 n2 cnt = EdgeStatement ent attr
    where ent  = [ENodeId NoEdge (mkId n1), ENodeId DirectedEdge (mkId n2)]
          attr = [AttributeSetValue (StringId "label") (IntegerId cnt)]
  ns = S.toList $ nodes graph
  es = M.toList $ edges graph
  mkId n = NodeId (StringId (T.unpack n)) Nothing

--------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------
main :: IO ()
main = do
  _args <- parseArgs
  graph <- buildGraph `liftM` T.lines `liftM` T.hGetContents stdin
  let dotGraph = convertToDot graph
  putStrLn $ show (prettyPrintDot dotGraph)

parseArgs :: IO [String]
parseArgs = do
  args <- getArgs
  case args of
    [] -> return args
    _  -> putStrLn "usage: graph-trace" >> exitFailure

