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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Vary
  ( -- * Core type definition
    Vary,
    (:|),

    -- * Construction and Destruction:
    from,
    into,
    intoOnly,

    -- * 'pattern' matching:
    on,
    exhaustiveCase,
    defaultCase,

    -- * Transforming
    mapOn,
    mapOn',
    mapOn3,
    Mapped,
    morph,
    morphed,

    -- * Informational
    size,
    activeIndex,
  )
where

import Control.Monad (guard)
import Data.Function ((&))
import Data.Kind
-- import qualified Data.Vector.Unboxed as UVector
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Vary.Core (Vary (..), popVary)
import Vary.Utils

size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx

-- | Put a value into a Vary
--
-- In the case of duplicate types,
-- uses the first matching type index.
from ::
  forall a l. a :| l => a -> Vary l
{-# INLINE from #-}
from = fromAt @(IndexOf a l)

-- | Attempts to turn the Vary back into a particular type.
--
-- This might fail since the Vary might actually contain another possibility,
-- which is why a `Maybe` is returned.
--
-- If you have a single possibility, you can use `intoOnly`.
into :: forall a l. (a :| l) => Vary l -> Maybe a
{-# INLINE into #-}
into = intoAt @(IndexOf a l)

-- | Extract the value of a variant with one possibility.
--
-- A variant with only a single possibility
-- can always be safely turned back into this one type.
--
-- If you have multiple possibilities, use `into`.
intoOnly :: forall a. Vary '[a] -> a
{-# INLINE intoOnly #-}
intoOnly (Vary _ val) = unsafeCoerce val

-- | Base case of an exhaustive pattern match. Use it together with `on`.
--
-- Since it is impossible to actually _construct_ a value of the type `Vary '[]`,
-- we can "turn it into anything", just like `Data.Void.absurd`.
exhaustiveCase :: forall anything. Vary '[] -> anything
{-# INLINE exhaustiveCase #-}
exhaustiveCase _vary =
  error "exhaustiveCase was unexpectedly called!"

-- | Base case of a non-exhaustive pattern match. Use it together with `on`.
--
-- If you've handled the variants you like and have some left,
-- you can specify a default fallback value using `defaultCase`.
--
-- Indeed, this function is just another name for `const`.
defaultCase :: forall a l. a -> Vary l -> a
{-# INLINE defaultCase #-}
defaultCase = const

-- | Extend a smaller Vary into a bigger one, or change the order of its elements.
--
-- This is a O(1) operation, as the tag number stored in the variant is
-- changed to the new tag number.
--
-- In many cases GHC can even look through the old->new Variant structure entirely,
-- and e.g. inline the variant construction all-together.
morph :: forall xs ys. (Subset xs ys) => Vary xs -> Vary ys
morph = morph' @xs @ys

fromAt ::
  forall (n :: Nat) (l :: [Type]).
  ( KnownNat n
  ) =>
  Index n l ->
  Vary l
{-# INLINE fromAt #-}
fromAt a = Vary (natValue @n) (unsafeCoerce a)

intoAt ::
  forall (n :: Nat) (l :: [Type]).
  ( KnownNat n
  ) =>
  Vary l ->
  Maybe (Index n l)
{-# INLINE intoAt #-}
intoAt (Vary t a) = do
  guard (t == natValue @n)
  return (unsafeCoerce a)

-- | Handle a particular variant possibility.
--
-- This is the main way to do case analysis (or 'deconstruct') a variant.
--
-- Use it together with `exhaustiveCase` if you handle all possibilities,
-- or `defaultCase` if you don't want to.
--
-- Even though in many cases GHC is able to infer the types,
-- it is a good idea to combine it with `TypeApplications`:
--
-- > -- Note that GHC can infer this type without problems:
-- > -- example :: Vary (Bool : Int : l) -> String
-- > example =
-- >   Vary.defaultCase "other value"
-- >   & Vary.on @Bool show
-- >   & Vary.on @Int (\x -> show (x + 1))
on :: forall a b l. (a -> b) -> (Vary l -> b) -> Vary (a : l) -> b
{-# INLINE on #-}
on thisFun restFun vary =
  case Vary.into @a vary of
    Just val -> thisFun val
    Nothing ->
      restFun (coerceHigher vary)
  where
    -- Invariant: does not contain @a
    {-# INLINE coerceHigher #-}
    coerceHigher :: Vary (a : l) -> Vary l
    coerceHigher (Vary idx val) =
      unsafeCoerce (Vary (idx - 1) val)

-- -- mapOn :: forall a b l. (a -> b) -> Vary (a ': l) -> Vary (b ': l)
-- mapOn :: forall a b l1 l2. (a :| l1, b :| l2, Subset l1 l2) => (a -> b) -> Vary l1 -> Vary l2 
-- mapOn fun vary = on @a (from @b . fun) morph (morph vary)

mapOn3 :: forall a b l. (a -> b) -> Vary (a : l) -> Vary (b : l)
mapOn3 fun vary = case Vary.Core.popVary vary of
  Right val -> from @b (fun val)
  Left other -> morph other

mapOn :: forall a b xs ys. (a :| xs, Mapped a b xs ys) => (a -> b) -> Vary xs -> Vary ys
mapOn = mapVary @a @b @xs @ys

mapOn' :: forall a b xs ys. (a :| xs) => (a -> b) -> Vary xs -> Vary (ReplaceN (IndexOf a xs) b ys)
mapOn' = undefined

-- mapOn :: forall a b xs ys. (a :| xs, b :| ys, ys ~ Mapped a b xs) => (a -> b) -> Vary xs -> Vary ys
-- mapOn fun vary = case into @a vary of
--   Nothing -> unsafeCoerce vary
--   Just a -> from @b (fun a)

-- | Execute a function expecting a larger (or differently-ordered) variant
--
-- with a smaller (or differently-ordered) variant,
-- by calling `morph` on it before running the function.
morphed :: forall a b res. (Subset a b) => (Vary b -> res) -> Vary a -> res
{-# INLINE morphed #-}
morphed fun = fun . morph
