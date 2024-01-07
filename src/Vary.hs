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
    TypeOperators,
    AllowAmbiguousTypes
#-}

module Vary where

import Data.Kind
import GHC.Exts (Any)
import GHC.TypeLits
import Vary.Utils
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (guard)

data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any


size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx

-- | Put a value into a Variant
--
-- Use the first matching type index.
into :: forall a l.
   (a :| l
   ) => a -> Vary l
{-# INLINABLE into #-}
into = intoAt @(IndexOf a l)

from :: forall a l. (a :| l) => Vary l -> Maybe a
from = fromAt @(IndexOf a l)

morph :: forall a b. Subset a b => Vary a -> Vary b
morph (Vary idx val) = Vary newIdx val
  where
   mapping = reifyIndices @a @b
   newIdx = fromIntegral (mapping !! fromIntegral idx)

intoAt :: forall (n :: Nat) (l :: [Type]).
   ( KnownNat n
   ) => Index n l -> Vary l
{-# INLINABLE intoAt #-}
intoAt a = Vary (natValue' @n) (unsafeCoerce a)

fromAt :: forall (n :: Nat) (l :: [Type]).
   ( KnownNat n
   ) => Vary l -> Maybe (Index n l)
{-# INLINABLE fromAt #-}
fromAt (Vary t a) = do
   guard (t == natValue' @n)
   return (unsafeCoerce a)