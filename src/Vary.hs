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
    morph,
    morphed,
  )
where

import Control.Monad (guard)
import Data.Function ((&))
import Data.Kind
import qualified Data.Vector.Unboxed as UVector
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Vary.Core (Vary (..))
import Vary.Utils

-- $setup
-- >>> :set -XGHC2021
-- >>> :set -XDataKinds
-- >>> import qualified Vary
-- >>> import Data.Function ((&))

-- | Builds a Vary from the given value.
--
-- >>> let thingy :: Vary '[Bool, Char]; thingy = Vary.from 'a'
-- >>> thingy
-- Vary.from 'a'
--
-- In the case of number literals or (with OverloadedStrings or OverloadedLists enabled) string or list literals,
-- it might be necessary to include a TypeApplication.
-- In most other cases, GHC is able to infer which possibility to use (though you might still like type applications even here for improved readability).
--
-- >>> Vary.from @Int 42 :: Vary '[Int, String]
-- Vary.from 42
--
-- In the case of the Vary contains duplicate types,
-- the first matching type index is used.
from ::
  forall a l. a :| l => a -> Vary l
{-# INLINE from #-}
from = fromAt @(IndexOf a l)

-- | Attempts to turn the Vary back into a particular type.
--
-- This might fail since the Vary might actually contain another possibility,
-- which is why a `Maybe` is returned.
--
-- If you have a single possibility, you can use `intoOnly` instead.
--
-- == Polymorphic functions
--
-- If you pass the result to a polymorphic function, GHC might not be able to infer which result type you'd like to try to extract.
-- Indicate the desired result type using a TypeApplication:
--
-- >>> let vary = Vary.from @Bool True :: Vary '[Bool, String]
-- >>> Vary.into @Bool vary
-- Just True
--
-- == Type errors
-- Sometimes you might see nasty long type errors, containing the string
-- `Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location`.
--
-- This happens when other parts of your code keep the type list fully abstract (only use the `:|` constraint).
--
-- You can fix it by either giving a type to an intermediate value,
-- or by passing a second type application to this function:
--
-- >>> let vary = if True then Vary.from True else Vary.from 'a' -- Inferred type: `Bool :| l, Char :| l => Vary l`
-- >>> Vary.into @Bool @(Char : Bool : _) vary
-- Just True
--
-- As you can see from the above example, it is often not necessary to specify the /full/ type list.
-- A prefix is commonly enough.
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
-- Since it is impossible to actually /construct/ a value of the type @Vary '[]@,
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

-- | Extend a smaller `Vary` into a bigger one, change the order of its elements, or get rid of duplicates.
--
-- === Extend a smaller `Vary`:
-- >>> small = Vary.from True :: Vary '[Bool]
-- >>> big = Vary.morph small :: Vary '[Bool, Int, String]
-- >>> big
-- Vary.from True
--
-- === Reorder elements:
-- >>> foo = Vary.from @Int 42 :: Vary '[Bool, Int]
-- >>> bar = Vary.morph foo    :: Vary '[Int, Bool]
--
-- === Get rid of duplicate elements:
-- >>> duplicates = Vary.from @Int 42       :: Vary '[Int, Int, Int]
-- >>> noduplicates = Vary.morph duplicates :: Vary '[Int]
--
-- == Efficiency
-- This is a O(1) operation, as the tag number stored in the variant is
-- changed to the new tag number.
--
-- In many cases GHC can even look through the old->new Variant structure entirely,
-- and e.g. inline the variant construction all-together.
morph :: forall ys xs. (Subset xs ys) => Vary xs -> Vary ys
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
-- This is the main way to 'deconstruct' or a variant.
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
-- >   Vary.on @Bool show
-- >   $ Vary.on @Int (\x -> show (x + 1))
-- >   $ Vary.defaultCase "other value"
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

-- | Execute a function expecting a larger (or differently-ordered) variant
-- with a smaller (or differently-ordered) variant,
-- by calling `morph` on it before running the function.
morphed :: forall a b res. (Subset a b) => (Vary b -> res) -> Vary a -> res
{-# INLINE morphed #-}
morphed fun = fun . morph

-- | Run a function on one of the variant's possibilities, keeping all other possibilities the same.
--
-- This is the generalization of functions like Either's `Data.Either.mapLeft` and `Data.Either.mapRight`.
--
-- If you want to map a polymorphic function like `show` which could match more than one possibility,
-- use a TypeApplication to specify the desired possibility to match:
--
-- >>> :{
-- (Vary.from @Int 42           :: Vary '[Int, Bool] )
--   & Vary.mapOn @Bool show    -- Vary '[Int, String]
--   & Vary.mapOn @Int show     -- Vary '[String, String]
-- :}
-- Vary.from "42"
--
-- If you end up with a variant with multiple duplicate possibilities, use `morph` to join them:
--
-- >>> :{ 
-- (Vary.from True                :: Vary '[Char, Int, Bool])
--   & Vary.mapOn @Bool show      -- Vary '[Char, Int, String]
--   & Vary.mapOn @Int show       -- Vary '[Char, String, String]
--   & Vary.mapOn @Char show      -- Vary '[String, String, String]
--   & Vary.morph @'[String]      -- Vary '[String]
--   & Vary.intoOnly              -- String
-- :}
-- "True"
--
-- Note that if you end up handling all cases of a variant, you might prefer using `Vary.on` and `Vary.exhaustiveCase` instead.
--
-- == Generic code
--
-- It is possible to use the most general type of this function in your own signatures;
-- To do this, add the `Mappable` constraint (exposed from `Vary.Utils`) 
-- to relate the input variant with the output variant.
--
-- import qualified Data.Char
-- >>> :{
-- example4 :: (Vary.Utils.Mappable Int Bool xs ys, Vary.Utils.Mappable Char Int ys zs) => Vary xs -> Vary zs
-- example4 vary =
--   vary
--   & Vary.mapOn @Int (\x -> x > 0)
--   & Vary.mapOn @Char Data.Char.ord
-- :}
--
-- == Duplicate possibilities
-- Vary.mapOn will only work on the first instance of the type that is encountered.
-- This is only a problem if a possibility is in the list multiple times;
-- be sure to `Vary.morph` duplicate possibilities away if needed.
mapOn :: forall a b xs ys. (Mappable a b xs ys) => (a -> b) -> Vary xs -> Vary ys
mapOn fun vary@(Vary tag val) = 
  case into @a vary of
    Just a -> from @b (fun a)
    Nothing -> (Vary tag val)
