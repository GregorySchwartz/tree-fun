-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

module Types where

-- Built-in
import qualified Data.Map as M

-- Advanced
type DistanceMap a = M.Map a [(a, Int)]
