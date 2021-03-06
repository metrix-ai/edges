module Edges.Instances
where

import Edges.Prelude
import Edges.Types
import Edges.Functions
import Edges.Instances.Cereal ()


deriving instance Eq (Node a)
deriving instance Eq (Edges a b)

deriving instance Ord (Node a)

instance Show (Node a) where show (Node int) = show int
instance Show (NodeCounts a) where show = show . nodeCountsList
deriving instance Show (Edges a b)
deriving instance Show (Amount a)

instance NFData (Edges a b) where rnf a = seq a ()
deriving instance NFData (Node a)
instance NFData (NodeCounts a) where rnf a = seq a ()
instance NFData (EdgeCounts a b) where rnf a = seq a ()
deriving instance NFData (Amount a)
