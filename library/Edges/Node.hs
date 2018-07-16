module Edges.Node
(
  Node(..),
  genWithLimit,
)
where

import Edges.Prelude
import Edges.Types
import Edges.Cereal.Instances ()
import qualified Test.QuickCheck.Gen as Gen

instance Show (Node a) where
  show (Node int) = show int

deriving instance Eq (Node a)

deriving instance Ord (Node a)

genWithLimit :: Int -> Gen.Gen (Node a)
genWithLimit max = Node <$> Gen.choose (0, max)
