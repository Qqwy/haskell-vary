{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns,
                        ConstraintKinds,
                        DataKinds,
                        DeriveFunctor,
                        DeriveGeneric,
                        FlexibleContexts,
                        FlexibleInstances,
                        GADTs,
                        GeneralizedNewtypeDeriving,
                        LambdaCase,
                        MultiParamTypeClasses,
                        NoStarIsType,
                        RankNTypes,
                        RoleAnnotations,
                        ScopedTypeVariables,
                        StandaloneDeriving,
                        TupleSections,
                        TypeApplications,
                        TypeFamilies,
                        TypeOperators #-}

--{-# OPTIONS_HADDOCK not-home #-}
module Vary.Utils where

import Data.Kind
import GHC.TypeLits

class (e :: Type) :| (es :: [Type]) where
  -- | Get the position of @e@ in @es@.
  --
  -- /Note:/ GHC is kind enough to cache these values as they're top level CAFs,
  -- so the lookup is amortized @O(1)@ without any language level tricks.
  reifyIndex :: Int
  reifyIndex =
    -- Don't show "minimal complete definition" in haddock.
    error "reifyIndex"

instance TypeError
  ( Text "There is no alternative for '" :<>: ShowType e :<>: Text "' in the variant list"
  ) => e :| '[] where
  reifyIndex = error "unreachable"

instance {-# OVERLAPPING #-} e :| (e : es) where
  reifyIndex = 0

instance e :| es => e :| (x : es) where
  reifyIndex = 1 + reifyIndex @e @es


-- | Provide evidence that @xs@ is a subset of @es@.
class KnownPrefix es => Subset (xs :: [Type]) (es :: [Type]) where
  subsetFullyKnown :: Bool
  subsetFullyKnown =
    -- Don't show "minimal complete definition" in haddock.
    error "subsetFullyKnown"

  reifyIndices :: [Int]
  reifyIndices =
    -- Don't show "minimal complete definition" in haddock.
    error "reifyIndices"

-- If the subset is not fully known, make sure the subset and the base stack
-- have the same unknown suffix.
instance {-# INCOHERENT #-}
  ( KnownPrefix es
  , xs `IsUnknownSuffixOf` es
  ) => Subset xs es where
  subsetFullyKnown = False
  reifyIndices = []

-- If the subset is fully known, we're done.
instance KnownPrefix es => Subset '[] es where
  subsetFullyKnown = True
  reifyIndices = []

instance (e :| es, Subset xs es) => Subset (e : xs) es where
  subsetFullyKnown = subsetFullyKnown @xs @es
  reifyIndices = reifyIndex @e @es : reifyIndices @xs @es

----

-- | Calculate length of a statically known prefix of @es@.
class KnownPrefix (es :: [Type]) where
  prefixLength :: Int

instance KnownPrefix es => KnownPrefix (e : es) where
  prefixLength = 1 + prefixLength @es

instance {-# INCOHERENT #-} KnownPrefix es where
  prefixLength = 0

----

-- | Require that @xs@ is the unknown suffix of @es@.
class (xs :: [Type]) `IsUnknownSuffixOf` (es :: [Type])
instance {-# INCOHERENT #-} xs ~ es => xs `IsUnknownSuffixOf` es
instance xs `IsUnknownSuffixOf` es => xs `IsUnknownSuffixOf` (e : es)