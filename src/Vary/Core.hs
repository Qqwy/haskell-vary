{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Vary.Core (Vary (..), popVary) where

import Data.Kind (Type)
import GHC.Exts (Any)
import qualified Unsafe.Coerce as Data.Coerce
import Control.DeepSeq (NFData (..))

-- | Vary, contains one value out of a set of possibilities
--
-- Vary is what is known as a /Variant/ type.
-- This is also known as an /open union/ or /coproduct/, among other names.
--
-- You can see it as the generalization of `Either`.
-- Conceptually, these are the same:
--
-- > Vary '[a, b, c, d, e]
-- > Either a (Either b (Either c (Either d e)))
--
-- However, compared to a deeply nested `Either`, `Vary` is:
--
-- - Much easier to work with
-- - Much more efficient, as a single (strict) word is used for the tag.
--
-- `Vary`'s can be constructed with "Vary".`Vary.from` and values can be extracted using "Vary".`Vary.into` and "Vary".'Vary.on' .
data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any

emptyVaryError name = error (name <> " was called on empty Vary '[]")

popVary :: Vary (a ': as) -> Either (Vary as) a
{-# INLINE popVary #-}
popVary (Vary 0 val) = Right (Data.Coerce.unsafeCoerce val)
popVary (Vary tag val) = Left (Data.Coerce.unsafeCoerce (Vary (tag - 1) val))

instance Eq (Vary '[]) where
  (==) = emptyVaryError "Eq.(==)"

instance (Eq a, Eq (Vary as)) => Eq (Vary (a ': as)) where
    a == b = popVary a == popVary b

instance Ord (Vary '[]) where
    compare = emptyVaryError "Ord.compare"

instance (Ord a, Ord (Vary as)) => Ord (Vary (a ': as)) where
    l `compare` r = popVary l `compare` popVary r

instance Show (Vary '[]) where
    show = emptyVaryError "Show.show"

instance (Show a, Show (Vary as)) => Show (Vary (a ': as)) where
    show vary = case popVary vary of
        Right val -> "Vary.from " <> show val
        Left other -> show other

instance NFData (Vary '[]) where
    rnf = emptyVaryError "NFData.rnf"

instance (NFData a, NFData (Vary as)) => NFData (Vary (a ': as)) where
    rnf vary = rnf (popVary vary)
