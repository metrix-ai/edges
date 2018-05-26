module Edges.UnliftedArray.FoldM
where

import Edges.Prelude


{-|
This function is partial in the sense that it expects the
amount of produced elements to match the specified amount.
-}
sizedUnsafe :: (PrimMonad m, PrimUnlifted element) => Int -> FoldM m (Int, element) (UnliftedArray element)
sizedUnsafe size =
  FoldM step init extract
  where
    step mutable (index, element) =
      writeUnliftedArray mutable index element $> mutable
    init =
      unsafeNewUnliftedArray size
    extract mutable =
      unsafeFreezeUnliftedArray mutable
