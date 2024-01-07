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
    PolyKinds,
    UndecidableInstances,
    AllowAmbiguousTypes
#-}

module Vary where

import Data.Kind
import Data.Proxy
import GHC.Exts (Any)
import GHC.TypeLits

data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any


size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx


-- | Get list length
type family Length (xs :: [k]) :: Nat where
   Length xs = Length' 0 xs

type family Length' n (xs :: [k]) :: Nat where
   Length' n '[]       = n
   Length' n (x ': xs) = Length' (n+1) xs

natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))