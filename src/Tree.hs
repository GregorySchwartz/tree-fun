-- Tree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to trees

module Tree where

-- Built-in
import Data.Tree
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import Control.Applicative
import qualified Data.Foldable as F

-- Local
import Types

-- | Convert a bool to an integer
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- | Return the labels of the leaves of the tree
leaves :: Tree a -> [a]
leaves (Node { rootLabel = x, subForest = [] }) = [x]
leaves (Node { rootLabel = _, subForest = xs }) = concatMap leaves xs

-- | Return the labels of the leaves of the tree with their relative heights
-- from the root (the input number you give determines how many steps away the
-- leaves are, should almost always start at 0)
leavesHeight :: (Ord a) => Int -> Tree a -> M.Map a Int
leavesHeight h (Node { rootLabel = x, subForest = [] }) = M.singleton x h
leavesHeight h (Node { rootLabel = _, subForest = xs }) =
    M.unions . map (leavesHeight (h + 1)) $ xs

-- | Return the labels of the leaves of the tree with their relative heights
-- from the root (the input number you give determines how many steps away the
-- leaves are, should almost always start at 0), slower version not requiring
-- Ord but no Maps
leavesHeightList :: Int -> Tree a -> [(a, Int)]
leavesHeightList h (Node { rootLabel = x, subForest = [] }) = [(x, h)]
leavesHeightList h (Node { rootLabel = _, subForest = xs }) =
    concatMap (leavesHeightList (h + 1)) xs

-- | Return the inner nodes of the tree
innerNodes :: Tree a -> [a]
innerNodes (Node { rootLabel = _, subForest = [] }) = []
innerNodes (Node { rootLabel = x, subForest = xs }) = x
                                                    : concatMap innerNodes xs

-- | Return the map of distances from each leaf to another leaf
getDistanceMap :: (Eq a, Ord a) => Tree a -> DistanceMap a
getDistanceMap tree = M.fromListWith (M.unionWith (S.><))
                    $ (\x y -> if x == y
                                   then (x, M.singleton 0 (S.singleton y))
                                   else ( x
                                        , M.singleton
                                          (getDistance tree x y)
                                          (S.singleton y) ) )
                  <$> leaves tree
                  <*> leaves tree

-- | Find the distance between two leaves in a tree.
getDistance :: (Eq a) => Tree a -> a -> a -> Int
getDistance (Node { rootLabel = l, subForest = [] }) x y = boolToInt
                                                         $ l `elem` [x, y]
getDistance n@(Node { rootLabel = _, subForest = xs }) x y
    | none      = 0
    | otherwise = sum
                . (:) (boolToInt notShared)
                . map (\t -> getDistance t x y)
                $ xs
  where
    -- Only count nodes that have one or the other, not shared or empty
    notShared = (elem x ls) || (elem y ls) && not (elem x ls && elem y ls)
      where
        ls = leaves n
    none = not (elem x ls || elem y ls)
      where
        ls = leaves n

-- | Find the distance between two leaves in a leafNode tree. Begin recording
-- distances when record is True (should have height starting at 0)
getDistanceSuperNode :: (Eq a, Ord a) => Tree (SuperNode a) -> a -> a -> Int
getDistanceSuperNode (Node { rootLabel = SuperNode { myLeaves = ls
                                                   , myParent = p }
                                  , subForest = ts } ) x y
    | shared ls    = head
                   . filter (/= 1)
                   . map (\a -> getDistanceSuperNode a x y)
                   $ ts
    | notShared ls = getParentLeafDist x p + getParentLeafDist y p
    | otherwise    = 0
  where
    -- Only count nodes that have one or the other, not shared or empty
    notShared xs = (M.member x xs || M.member y xs)
                && not (M.member x xs && M.member y xs)
    shared xs    = M.member x xs && M.member y xs
    getParentLeafDist a b = fromJust . M.lookup a . myLeaves $ b

-- | Get the sum of a tree for a tree with numbered labels
sumTree :: (Num a) => Tree a -> a
sumTree = F.foldl' (+) 0

-- | Convert a tree to the LeafNode tree data structure (the leaves are in the
-- nodes)
toSuperNodeTree :: (Ord a) => SuperNode a -> Tree a -> Tree (SuperNode a)
toSuperNodeTree p n@(Node { rootLabel = x, subForest = xs }) =
    Node { rootLabel = SuperNode { myRootLabel = x
                                 , myLeaves = leavesHeight 0 n
                                 , myParent = p }
         , subForest = map (toSuperNodeTree p) xs }
