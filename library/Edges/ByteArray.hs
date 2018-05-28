module Edges.ByteArray
where

import Edges.Prelude
import Data.Primitive


fold :: Prim prim => ByteArray -> Unfold prim
fold ba =
  let
    !primSize =
      8 * sizeofByteArray ba
    in
      Unfold $ \ step init ->
      let
        loop index !state =
          if index < primSize
            then loop (succ index) (step state (indexByteArray ba index))
            else state
        in loop 0 init
