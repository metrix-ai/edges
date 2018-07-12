module Edges.Edges
(
  Edges,
  list,
)
where

import Edges.Prelude
import Edges.Internal.Types
import qualified PrimitiveExtras.Monad as A
import qualified Control.Foldl as B


list :: [(Node a, Node b)] -> Edges a b
list list =
  let
    remappedList = flip fmap list $ \ (Node aInt, Node bInt) -> (aInt, fromIntegral bInt)
    aAmount = length (nub (map fst remappedList))
    bAmount = length (nub (map snd remappedList))
    in
      pseq (par aAmount bAmount) $
      Edges bAmount $ runIdentity $ A.primMultiArray aAmount $ \ fold ->
      Identity $ B.fold fold remappedList
