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

data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any


size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx

-- | Put a value into a Variant
--
-- Use the first matching type index.
into :: forall a l.
   (a :|| l
   ) => a -> Vary l
{-# INLINABLE into #-}
into = intoAt @(IndexOf a l)


intoAt :: forall (n :: Nat) (l :: [Type]).
   ( KnownNat n
   ) => Index n l -> Vary l
{-# INLINABLE intoAt #-}
intoAt a = Vary (natValue' @n) (unsafeCoerce a)