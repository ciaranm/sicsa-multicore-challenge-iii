-- MaxClique.hs
--
-- Author: Patrick Maier <Patrick.Maier@glasgow.ac.uk>
-- Date: 10 June 2014
--
-- A simple sequential Haskell implementation of the max clique algorithm 
-- described in [1], representing sets using Patricia trees (Data.IntSet).
-- 
-- compile: ghc --make -O3 MaxClique.hs
-- run:     ./MaxClique FILE
--
-- References:
-- [1] Ciaran McCreesh. Algorithms for the Maximum Clique Problem,
--     and How to Parallelise Them. 6 June 2014.
--
---------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude
import Control.Category (Category, (<<<))
import qualified Control.Category as Cat (id, (.))
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.Array (Array, bounds, (!), listArray)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import qualified Data.IntSet as VertexSet
       (fromAscList, null, size, member,
        minView, delete, intersection, difference)
import Data.List (group, groupBy, sort, sortBy)
import qualified Data.IntMap.Strict as StrictMap (fromAscList, findWithDefault)
import Data.Ord (comparing, Down(Down))
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)


---------------------------------------------------------------------------
-- permutations

-- A bijection from 'a' to 'b' is a pair of functions 'eff :: a -> b' and
-- 'gee :: b -> a' that are inverse to each other, i.e. 'gee . eff = id' and
-- 'eff . gee = id'.
data Bijection a b = Bijection { eff :: a -> b, gee :: b -> a }

-- Bijections form a category.
instance Category Bijection where
  id = Bijection { eff = \ x -> x,
                   gee = \ x -> x }
  beta . alpha = Bijection { eff = \ x -> eff beta $ eff alpha x,
                             gee = \ z -> gee alpha $ gee beta z }

-- Bijections can be inverted.
inv :: Bijection a b -> Bijection b a
inv alpha = Bijection { eff = gee alpha, gee = eff alpha }

-- Bijections can be applied.
app :: Bijection a b -> a -> b
app alpha x = eff alpha x

-- Permutations are endo-bijections.
type Perm a = Bijection a a


---------------------------------------------------------------------------
-- vertices

-- Vertices are non-negative machine integers.
type Vertex = Int

-- Hilbert's Hotel permutation on vertices: 'eff' increments, 'gee' decrements.
permHH :: Perm Vertex
permHH = Bijection { eff = \ v -> v + 1,
                     gee = \ v -> if v > 0 then v - 1 else error "permHH.gee" }


---------------------------------------------------------------------------
-- undirected graphs

-- An undirected graph is represented as a list of non-empty lists of vertices
-- such that each vertex occurs exactly once as the head of a list, and no list
-- contains duplicates. The list of heads is the list of vertices of the
-- undirected graph, and the tails are the respective vertices' adjacency lists.
-- The list of vertices and all adjacency lists are sorted in ascending order.
-- Additionally, an undirected graph is symmetric, ie. u is in the adjecency
-- list of v iff v is in the adjacency list of u.
type UGraph = [[Vertex]]

-- Convert a list of edges over 'n' vertices (numbered '1' to 'n') into an
-- undirected graph.
mkUG :: Int -> [(Vertex,Vertex)] -> UGraph
mkUG n edges =
  if n == length uG then uG else error "mkUG: vertices out of bounds"
    where
      sortUniq = map head . group . sort
      refl_edges = [(v,v) | v <- [1 .. n]]
      symm_edges = concat [[(u,v), (v,u)] | (u,v) <- edges]
      all_edges = sortUniq (refl_edges ++ symm_edges)
      grouped_edges = groupBy (\ (u,_) (v,_) -> u == v) all_edges
      uG = [u : [v | (_,v) <- grp, v /= u] | grp@((u,_):_) <- grouped_edges]

-- Apply a vertex permutation to an undirected graph.
appUG :: Perm Vertex -> UGraph -> UGraph
appUG alpha =
  sortBy (comparing head) .          -- sort list of vertices
  map (\ (u:vs) -> (u : sort vs)) .  -- sort all adjacency lists
  map (map $ app alpha)              -- apply alpha to all vertices

verticesUG :: UGraph -> [Vertex]
verticesUG = map head

degreesUG :: UGraph -> [(Vertex,Int)]
degreesUG = map (\ (u:vs) -> (u, length vs))

-- Check degrees are anti-monotonic wrt. vertex order, ie. whether
-- 'map snd . degreesUG' yields a non-increasing list.
isDegreesAntiMonotonicUG :: UGraph -> Bool
isDegreesAntiMonotonicUG = isAntiMonotonic . map snd . degreesUG
  where
    isAntiMonotonic (x:y:zs) = x >= y && isAntiMonotonic (y:zs)
    isAntiMonotonic [_]      = True
    isAntiMonotonic []       = True

-- Compute a permutation that transforms the given undirected graph into
-- an isomorphic one where the degrees are anti-monotonic wrt. vertex order.
-- This resulting vertex order is also known as /non-increasing degree order/.
antiMonotonizeDegreesPermUG :: UGraph -> Perm Vertex
antiMonotonizeDegreesPermUG uG =
  Bijection { eff = f, gee = g }
    where
      cmp = comparing $ \ (v, d) -> (Down d, v)
      g_assocs = zip (verticesUG uG) (map fst $ sortBy cmp $ degreesUG uG)
      f_assocs = sort [(x,y) | (y,x) <- g_assocs]
      !f_map = StrictMap.fromAscList f_assocs
      !g_map = StrictMap.fromAscList g_assocs
      f x = StrictMap.findWithDefault x x f_map
      g y = StrictMap.findWithDefault y y g_map


---------------------------------------------------------------------------
-- DIMACS2 parser

-- Parse DIMACS2 format and return number vertices 'n' and a list of edges;
-- vertices appearing in the list of edges are positive and bounded by 'n'.
parseDIMACS2 :: String -> (Int, [(Vertex,Vertex)])
parseDIMACS2 input =
  case stripDIMACS2Comments (map words $ lines input) of
    []   -> error "parseDIMACS2: no data"
    p:es -> (n, edges)
              where
                n = parseDIMACS2PLine p
                edges = map (parseDIMACS2ELine n) es

stripDIMACS2Comments :: [[String]] -> [[String]]
stripDIMACS2Comments = filter (\s -> not (null s || head (head s) == 'c'))

parseDIMACS2PLine :: [String] -> Int
parseDIMACS2PLine ("p":"edge":s:_) = n
  where
    n = case reads s of
          [(i,"")] -> if i > 0
                        then i
                        else error "parseDIMACS2: \"p\" line out of bounds (vertices)"
          _        -> error "parseDIMACS2: \"p\" line read error"
parseDIMACS2PLine _ = error "parseDIMACS2: \"p edge\" line missing or wrong"

parseDIMACS2ELine :: Int -> [String] -> (Vertex,Vertex)
parseDIMACS2ELine n  ["e", s1, s2] = (u,v)
  where
    u = case reads s1 of
          [(i,"")] -> if 1 <= i && i <= n
                        then i
                        else error "parseDIMACS2: \"e\" line out of bounds (source)"
          _        -> error "parseDIMACS2: \"e\" line read error (source)"
    v = case reads s2 of
          [(i,"")] -> if 1 <= i && i <= n
                        then i
                        else error "parseDIMACS2: \"e\" line out of bounds (sink)"
          _        -> error "parseDIMACS2: \"e\" line read error (sink)"
parseDIMACS2ELine _ _ = error "parseDIMCAS2: \"e\" line wrong"


---------------------------------------------------------------------------
-- vertex sets, represented as sets of Ints

type VertexSet = IntSet
  -- Supports operations fromAscLit, null, minView, delete,
  -- intersection, difference.


---------------------------------------------------------------------------
-- graphs, representation for max clique search

-- An undirected graph is represented as an array of adjacency sets,
-- which may be viewed as an adjacency matrix (of Booleans). This matrix
-- is assumed to be symmetric and irreflexive. Vertices are numbered from 0.
newtype Graph = G (Array Vertex VertexSet) deriving (Eq, Ord, Show, NFData)

-- Converts an undirected graph (whose vertices must be numbered from 0)
-- into the array of adjacency sets representation.
mkG :: UGraph -> Graph
mkG [] = error "mkG: empty graph"
mkG uG = if head (head uG) == 0 
           then G $ listArray (0, n - 1) [VertexSet.fromAscList vs | _:vs <- uG]
           else error "mkG: vertices not numbered from 0"
             where
               n = length uG

verticesG :: Graph -> [Vertex]
verticesG (G g) = [l .. u] where (l,u) = bounds g

adjacentG :: Graph -> Vertex -> VertexSet
adjacentG (G g) v = g ! v

isAdjacentG :: Graph -> Vertex -> Vertex -> Bool
isAdjacentG bigG u v = VertexSet.member v $ adjacentG bigG u

degreeG :: Graph -> Vertex -> Int
degreeG bigG = VertexSet.size . adjacentG bigG


---------------------------------------------------------------------------
-- greedy colouring

-- Ordered vertex colouring, reprented as a list of vertex-colour pairs
-- (where colours are positive integers).
type ColourOrder = [(Vertex,Int)]

-- Greedy colouring of a set of vertices, as in [1]. Returns the list of
-- vertex-colour pairs in reverse order of colouring, i.e. the head of the
-- list is the vertex coloured last (with the highest colour).
colourOrder :: Graph -> VertexSet -> ColourOrder
colourOrder bigG bigP = colourFrom 1 bigP []
  where
    -- outer while loop; 'coloured' is a partial colouring
    colourFrom :: Int -> VertexSet -> ColourOrder -> ColourOrder
    colourFrom !colour uncoloured coloured =
      if VertexSet.null uncoloured
        then coloured
        else greedyWith colour uncoloured coloured uncoloured
    -- inner while loop; 'coloured' is a partial colouring
    greedyWith :: Int -> VertexSet -> ColourOrder -> VertexSet -> ColourOrder
    greedyWith !colour uncoloured coloured colourable =
      case VertexSet.minView colourable of
        Nothing ->
          -- 'colourable' is empty
          colourFrom (colour + 1) uncoloured coloured
        Just (v, colourable_minus_v) ->
          -- 'v' is minimal vertex in 'colourable'
          greedyWith colour uncoloured' coloured' colourable'
            where
              coloured'   = (v,colour):coloured
              uncoloured' = VertexSet.delete v uncoloured
              colourable' = VertexSet.difference colourable_minus_v $
                                                 adjacentG bigG v


---------------------------------------------------------------------------
-- clique expansion

-- Branch-and-bound maxclique search, as in [1].
-- Takes the input graph, the current bound and incumbent, the currently
-- explored clique 'bigCee', and a set of candidate vertices 'bigPee' (all
-- connected to all vertices in 'bigCee') to be added to extensions of 'bigCee'.
-- Returns the maximum clique extending 'bigCee' by vertices in 'bigPee',
-- the clique size, and the number of calls to 'expand' (including this one).
expand :: Graph                -- input graph
       -> ([Vertex],Int)       -- current best solution (incumbent and bound)
       -> ([Vertex],Int)       -- currently explored solution (clique and size)
       -> VertexSet            -- candidate vertices (to add to current clique)
       -> ([Vertex],Int,Int64) -- result: max clique, size, calls
expand bigG incumbent_bound bigCee_size bigPee =
  loop 1 incumbent_bound bigCee_size bigPee $ colourOrder bigG bigPee
    where
      -- for loop
      loop :: Int64                   -- calls to 'expand'
           -> ([Vertex], Int)         -- incumbent and bound
           -> ([Vertex], Int)         -- current clique with size
           -> VertexSet               -- candidate vertices
           -> ColourOrder             -- ordered colouring of candidate vertices
           -> ([Vertex], Int, Int64)  -- result: max clique, size, calls
      loop !calls (incumbent,!bound) _            _    []                =
        (incumbent,bound,calls)
      loop !calls (incumbent,!bound) (bigC,!size) bigP ((v,colour):more) =
        if size + colour <= bound
          then (incumbent,bound,calls)
          else let
            -- accept v
            (bigC',!size')       = (v:bigC, size + 1)
            (incumbent',!bound') = if size' > bound
                                     then (bigC',    size')  -- new incumbent!
                                     else (incumbent,bound)
            bigP' = VertexSet.intersection bigP $ adjacentG bigG v
            -- recurse (unless bigP' empty)
            (incumbent'',!bound'',!rcalls) =
              if VertexSet.null bigP'
                then (incumbent',bound',0)
                else expand bigG (incumbent',bound') (bigC',size') bigP'
            -- reject v
            bigP'' = VertexSet.delete v bigP
            -- continue the loop (totting up calls and recursive calls)
          in loop (calls + rcalls) (incumbent'',bound'') (bigC,size) bigP'' more


---------------------------------------------------------------------------
-- max clique computation (without normalizing input to non-inc deg order)

maximumClique :: Graph              -- input graph
              -> ([Vertex], Int64)  -- result: max clique, #calls to 'expand'
maximumClique bigG = (max_clique, calls)
  where
    (max_clique, _, !calls) = expand bigG ([],0) ([],0) bigP
    bigP = VertexSet.fromAscList $ verticesG bigG


---------------------------------------------------------------------------
-- verification (of clique property, not of maximality)

-- True iff the given list of vertices form a clique.
isClique :: Graph -> [Vertex] -> Bool
isClique bigG vertices =
  and [isAdjacentG bigG u v | u <- vertices, v <- vertices, u /= v]


---------------------------------------------------------------------------
-- main (and aux stuff)

toFractional :: (Real a, Fractional b) => a -> b
toFractional = fromRational . toRational

-- time an IO action
timeIO :: IO a -> IO (a, NominalDiffTime)
timeIO action = do t0 <- getCurrentTime
                   x <- action
                   t1 <- getCurrentTime
                   return (x, diffUTCTime t1 t0)

usage :: IO a
usage = do
  putStrLn "Usage: MaxCliqueIntSet FILE"
  putStrLn "   or: MaxCliqueIntSet -[no]perm [FILE]"
  exitFailure
  
-- Expects one argument which must be the filename of a graph in DIMACS2 format.
-- Parses the file, computes and prints a maximum clique.
main :: IO ()
main = do
  -- parsing command line arguments
  args <- getArgs
  (permute, filename) <- case args of
                           ["-noperm", s] -> return (False, s)
                           ["-perm",   s] -> return (True,  s)
                           [_,         _] -> usage
                           ["-noperm"]    -> return (False, "")
                           ["-perm"]      -> return (True,  "")
                           ['-':_]        -> usage
                           [s]            -> return (True,  s)
                           _              -> usage
  -- reading input graph
  (uG, t_read) <- timeIO $ do
    input <- if null filename then getContents else readFile filename
    let (n, edges) = parseDIMACS2 input
    let uG' = mkUG n edges
    evaluate (rnf uG')
    return uG'
  putStrLn $ "t_read: " ++ show t_read
  -- printing some statistics
  let degrees = map snd $ degreesUG uG
  let n = length degrees
  let min_deg = minimum degrees
  let max_deg = maximum degrees
  let avg_deg = sum (map toFractional degrees) / toFractional n :: Double
  putStrLn $ "vertices: " ++ show n
  putStrLn $ "max degree: " ++ show max_deg
  putStrLn $ "min degree: " ++ show min_deg
  putStrLn $ "avg degree: " ++ show avg_deg
  -- permuting and converting input graph
  ((alpha, bigG), t_permute) <- timeIO $ do
    let alpha' | permute   = antiMonotonizeDegreesPermUG uG
               | otherwise = Cat.id
    let uG_alpha = appUG (inv permHH <<< alpha') uG
        -- uG_alpha in non-decreasing degree order, vertices numbered from 0.
    let bigG' = mkG uG_alpha
    evaluate (rnf bigG')
    return (alpha', bigG')
--BEGIN DEBUG
--let new_vertices = verticesG bigG
--let new_degrees  = map (degreeG bigG) new_vertices
--let old_vertices = map (app (inv alpha <<< permHH)) new_vertices
--putStrLn $ unlines $ map show $ zip3 new_vertices old_vertices new_degrees
--END DEBUG
  if permute
    then putStrLn $ "t_permute: " ++ show t_permute
    else putStrLn $ "t_construct: " ++ show t_permute
  -- computing max clique
  ((bigCstar, calls), t_compute) <- timeIO $ do
    let (bigCstar', !calls') = maximumClique bigG
    evaluate (rnf bigCstar')
    return (bigCstar', calls')
  -- print solution and statistics
  let bigCstar_alpha_inv = map (app (inv alpha <<< permHH)) bigCstar
  putStrLn $ "     C*: " ++ show bigCstar_alpha_inv
  putStrLn $ "sort C*: " ++ show (sort bigCstar_alpha_inv)
  putStrLn $ "size: " ++ show (length bigCstar)
  putStrLn $ "isClique: " ++ show (isClique bigG bigCstar)
  putStrLn $ "colourings: " ++ show calls
  putStrLn $ "t_compute: " ++ show t_compute
  exitSuccess
