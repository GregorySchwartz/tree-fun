-- Tree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to trees

module Tree where

-- Built-in
import Data.Tree
import qualified Data.Map as M
import Control.Applicative

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

-- | Return the map of distances from each leaf to another leaf
getDistanceMap :: (Eq a, Ord a) => Tree a -> DistanceMap a
getDistanceMap tree = M.fromListWith (++)
                    $ (\x y -> if x == y
                                   then (x, [(y, 0)])
                                   else(x, [(y, getDistance tree x y)]))
                  <$> leaves tree
                  <*> leaves tree

-- | Find the distance between two leaves in a tree. Begin recording distances
-- when record is True
getDistance :: (Eq a) => Tree a -> a -> a -> Int
getDistance (Node { rootLabel = l, subForest = [] }) x y = boolToInt
                                                         $ l `elem` [x, y]
getDistance n@(Node { rootLabel = _, subForest = xs }) x y
    = sum . (:) (boolToInt . notShared $ n) . map (\t -> getDistance t x y) $ xs
  where
    -- Only count nodes that have one or the other, not shared or empty
    notShared node = (elem x (leaves node) || elem y (leaves node))
                  && not (elem x (leaves node) && elem y (leaves node))

