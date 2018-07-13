module Edges.Node
(
  Node(..),
)
where

import Edges.Prelude
import Edges.Internal.Types

instance Show (Node a) where
  show (Node int) = show int

deriving instance Eq (Node a)

deriving instance Ord (Node a)
