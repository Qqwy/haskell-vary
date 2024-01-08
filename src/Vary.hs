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
{-# LANGUAGE NoStarIsType #-}

module Vary where

import Control.Monad (guard)
import Data.Function ((&))
import Data.Kind
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Vary.Utils

data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any

size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx

-- | Put a value into a Vary
--
-- In the case of duplicate types,
-- uses the first matching type index.
from ::
  forall a l.
  ( a :| l
  ) =>
  a ->
  Vary l
{-# INLINE from #-}
from = fromAt @(IndexOf a l)

-- | Attempts to turn the Vary back into a particular type.
--
-- This might fail since one of the other variants might be in there,
-- which is why a `Maybe` is returned.
into :: forall a l. (a :| l) => Vary l -> Maybe a
{-# INLINE into #-}
into = intoAt @(IndexOf a l)

-- | Extract the value of a variant with one possibility.
--
-- A variant with only a single possibility
-- can always be safely turned back into this one type.
intoOnly :: forall a. Vary '[a] -> a
{-# INLINE intoOnly #-}
intoOnly (Vary _ val) = unsafeCoerce val

case_ :: forall anything. Vary '[] -> anything
{-# INLINE case_ #-}
case_ _vary =
  -- Note that we ensure this is never called,
  -- because there is no way to construct a value of `Vary '[]`
  -- but GHC cannot be sure!
  undefined

defaultCase :: forall a l. a -> Vary l -> a
{-# INLINE defaultCase #-}
defaultCase = const

-- | Extend a smaller Vary into a bigger one, or change the order of its elements.
morph :: forall a b. (Subset a b) => Vary a -> Vary b
{-# INLINE morph #-}
morph (Vary idx val) = Vary newIdx val
  where
    mapping = reifyIndices @a @b
    newIdx = fromIntegral (mapping !! fromIntegral idx)

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
-- This is the main way to 'deconstruct' or a variant.
--
-- Use it together with `case_` if you handle all possibilities,
-- or `defaultCase` if you don't want to.
--
-- Even though in many cases GHC is able to infer the types,
-- it is a good idea to combine it with `TypeApplications`:
--
--  -- inferred type: `Vary (Bool : Int : l) -> String`
--  example =
--    defaultCase "other value"
--    & on @Bool show
--    & on @Int (\x -> show (x + 1))
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

-- | Flexible version of `on`, not restricted to match on the first possibility in the variant.
--
-- The advantage of this function is that you can add cases in any order.
-- The disadvantage is that GHC is unable to infer types when you use it,
-- so it will often complain that it needs more hints.
--
-- As such, you might prefer using `morph` to re-order a variant and then use the normal `on` on the result of that.
--
-- GHC is often unable to infer the type when using this,
-- and the error messages on failure are not very nice.
--
-- So be sure to be very clear in the type you expect to match.
on' :: forall a b l. (a :| l) => (a -> b) -> (Vary (Remove a l) -> b) -> Vary l -> b
on' thisFun restFun vary =
  case Vary.into @a vary of
    Just val -> thisFun val
    Nothing ->
      restFun (coerceRest vary)
  where
    -- Invariant: does not contain @a
    coerceRest :: Vary l -> Vary (Remove a l)
    coerceRest (Vary idx val) =
      if idx > natValue @(IndexOf a (a : l))
        then unsafeCoerce (Vary (idx - 1) val)
        else unsafeCoerce (Vary idx val)

-- example :: forall l. Vary  (Int : Bool : l) -> String
-- example :: Vary (Int : Bool : l) -> String
-- example :: Vary [Int, Bool] -> String
example :: Vary '[Bool, Int] -> String
example =
  -- defaultCase "hmm"
  case_
    & on boolFun
    & on intFun
    & morphed

boolFun :: Bool -> String
boolFun x = if x then "true" else "false"

intFun :: Int -> String
intFun y = if y < 0 then "negative" else "nonnegative"

-- unreachable
-- \$ on (\x -> if x then "true" else "false")
-- \$ on (\y -> "42")
-- on (\x -> x) unreachable

--   vary
--   & on (\x -> x)
--   vary
--   & on (\x -> if x then "true" else "false")
--   & unreachable
-- & unreachable

morphed :: forall a b res. (Subset b a) => (Vary a -> res) -> Vary b -> res
{-# INLINE morphed #-}
morphed fun vary = fun (morph vary)