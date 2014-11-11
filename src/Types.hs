-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

module Types where

-- Built-in
import qualified Data.Map as M
import qualified Data.Sequence as S

-- Algebraic
-- Tree with super smart nodes
data SuperNode a = Root | SuperNode { myRootLabel :: a
                                    , myParent    :: SuperNode a
                                    , myLeaves    :: M.Map a Int }
                                    deriving (Read, Show, Eq, Ord)

-- Basic
type Height = Int

-- Advanced
type DistanceMap a = M.Map a (M.Map Int (S.Seq a))
