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

data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any


popVary :: Vary (a ': as) -> Either (Vary as) a
{-# INLINE popVary #-}
popVary (Vary 0 val) = Right (Data.Coerce.unsafeCoerce val)
popVary (Vary tag val) = Left (Data.Coerce.unsafeCoerce (Vary (tag - 1) val))

instance Eq (Vary '[]) where
  (==) = undefined

instance (Eq a, Eq (Vary as)) => Eq (Vary (a ': as)) where
    a == b = popVary a == popVary b

instance Ord (Vary '[]) where
    compare = undefined

instance (Ord a, Ord (Vary as)) => Ord (Vary (a ': as)) where
    l `compare` r = popVary l `compare` popVary r

instance Show (Vary '[]) where
    show = undefined

instance (Show a, Show (Vary as)) => Show (Vary (a ': as)) where
    show vary = case popVary vary of
        Right val -> "(Vary.from " <> show val <> ")"
        Left other -> show other

instance NFData (Vary '[]) where
    rnf = undefined

instance (NFData a, NFData (Vary as)) => NFData (Vary (a ': as)) where
    rnf vary = rnf (popVary vary)
