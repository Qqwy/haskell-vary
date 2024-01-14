{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Vary.VEither (
  -- * Core type definition
  VEither(VLeft, VRight), 
  -- * Conversion
  toVary, 
  fromVary,
  fromLeft,
  fromRight,
  toEither,
  fromEither,
  veither,
  intoOnly,

  -- * case analysis ("pattern matching"):

  -- |
  --
  -- Besides the 'VLeft' and 'VRight' patterns,
  -- 'VEither' supports a bunch of handy combinator functions,
  -- similar to "Vary".'Vary.on' and co.
  onLeft,
  onRight,

  -- * Transforming
  mapLeftOn,
  mapRight,
  morph,
  morphed,
) where

import Control.Category ((>>>))
import Control.DeepSeq (NFData (..))
import qualified Data.Either
import Data.Kind (Type)
import Vary.Core (Vary(..))
import Vary.Utils (Subset, Mappable)
import Vary ((:|))
import qualified Vary
import GHC.Generics
import Unsafe.Coerce
import Data.Bifunctor

-- $setup
--
-- == General Usage
--
-- This module is intended to be used qualified:
--
-- >>> import Vary.VEither (VEither(VLeft, VRight))
-- >>> import qualified Vary.VEither as VEither
-- 
-- And for many functions, it is useful or outright necessary to enable the following extensions:
--
-- >>> :set -XGHC2021
-- >>> :set -XDataKinds

newtype VEither (errs :: [Type]) a = VEither (Vary (a : errs))

-- | Turns the 'VEither' into a normal Vary, no longer considering the @a@ a \'preferred\' value.
toVary :: VEither errs a -> Vary (a : errs)
{-# INLINE toVary #-}
toVary (VEither vary) = vary

-- | Turns a 'Vary' into a 'VEither'. Now the @a@ is considered the \'preferred\' value.
fromVary :: Vary (a : errs) -> VEither errs a
{-# INLINE fromVary #-}
fromVary vary = VEither vary

-- | Turns a 'VEither' into a normal 'Either'.
toEither :: VEither errs a -> Either (Vary errs) a
{-# INLINE toEither #-}
toEither = toVary >>> Vary.pop

-- | Turns a normal 'Either' into a 'VEither'.
fromEither :: Either (Vary errs) a -> VEither errs a
{-# INLINE fromEither #-}
fromEither = Data.Either.either Vary.morph Vary.from >>> fromVary

-- | Shorthand to construct a 'VEither' from a single error value.
--
-- Instead of:
--
-- >>> (VLeft (Vary.from @Bool True)) :: VEither '[Bool] String
-- VLeft (Vary.from @Bool True) 
--
-- You can just write:
--
-- >>> VEither.fromLeft @Bool True :: VEither '[Bool] String
-- VLeft (Vary.from @Bool True) 
fromLeft :: forall err errs a. err :| errs => err -> VEither errs a
fromLeft = Vary.from @err >>> VLeft

-- | Construct a 'VEither' from an @a@.
--
-- Exists for symmetry with 'fromLeft'.
-- Indeed, this is just another name for 'VRight'.
fromRight :: forall a errs. a -> VEither errs a
fromRight = VRight

-- | Case analysis on a 'VEither'. Similar to 'Data.Either.either'.
--
-- See also "VEither".'mapLeft', "VEither".'mapLeftOn' and "VEither".'mapRight'.
veither :: (Vary errs -> c) -> (a -> c) -> VEither errs a -> c
veither f _ (VLeft x)     =  f x
veither _ g (VRight y)    =  g y

{-# COMPLETE VLeft, VRight #-}

pattern VLeft :: forall a errs. Vary errs -> VEither errs a
{-# INLINE VLeft #-}
pattern VLeft errs <- (toEither -> Left errs)
   where
      VLeft (Vary tag err) = VEither ((Vary (tag+1) err))

pattern VRight :: forall a errs. a -> VEither errs a
{-# INLINE VRight #-}
pattern VRight a <- (toEither -> Right a)
  where
    VRight a = VEither (Vary.from @a a)

onLeft :: forall err b errs a. (err -> b) -> (VEither errs a -> b) -> VEither (err : errs) a -> b
onLeft thiserrFun restfun ve = case ve of
  VLeft e -> Vary.on @err thiserrFun (\otherErr -> restfun (VLeft otherErr)) e
  VRight a -> restfun (VRight a)

onRight :: (a -> b) -> (VEither errs a -> b) -> VEither errs a -> b
onRight valfun restfun ve = case ve of
  VRight a -> valfun a
  VLeft err -> restfun (VLeft err)

-- | If you have a VEither which does not actually contain any errors,
-- you can be sure it always contains an @a@.
--
-- Similar to "Vary".'Vary.intoOnly'.
intoOnly :: forall a. VEither '[] a -> a
intoOnly (VRight a) = a
intoOnly (VLeft emptyVary) = Vary.exhaustiveCase emptyVary


morph :: forall ys xs a. Subset (a : xs) (a : ys) => VEither xs a -> VEither ys a
morph = toVary >>> Vary.morph >>> fromVary

-- | Execute a function expecting a larger (or differently-ordered) variant
-- with a smaller (or differently-ordered) variant,
-- by calling `morph` on it before running the function.
morphed :: forall xs ys a res. Subset (a : xs) (a : ys) => (VEither ys a -> res) -> VEither xs a -> res
{-# INLINE morphed #-}
morphed fun = fun . morph

-- | Map a function over one of the error values inside the 'VEither'.
--
-- Any other 'VLeft' and  also 'VRight' are kept untouched.
--
-- Similar to "Vary".'Vary.mapOn'.
mapLeftOn :: forall x y xs ys a. (Mappable x y xs ys) => (x -> y) -> VEither xs a -> VEither ys a
mapLeftOn _ (VRight val) = VRight val
mapLeftOn fun (VLeft err) = VLeft $ Vary.mapOn fun err

-- | Map a function over the 'VEither' if it contains a 'VLeft', otherwise leave it alone.
--
-- See also "VEither".'mapLeftOn', "VEither".'mapRight' and "VEither".'veither'.
--
mapLeft :: (Vary xs -> Vary ys) -> VEither xs a -> VEither ys a
mapLeft fun ve = case ve of 
    VRight a -> VRight a
    VLeft errs -> VLeft (fun errs)

-- | Map a function over the 'VEither' if it contains a 'VRight', otherwise leave it alone.
--
-- Exists for symmetry with "VEither".'mapLeft' and "VEither".'mapLeftOn'.
--
-- Indeed, it is just another name for 'fmap'.
--
-- See also "VEither".'veither'.
mapRight :: (x -> y) -> VEither errs x -> VEither errs y
mapRight fun ve = case ve of 
    VRight a -> VRight (fun a)
    VLeft errs -> VLeft errs

instance (Show a, Show (Vary errs)) => Show (VEither errs a) where
  show (VLeft errs) = "VLeft (" <> show errs <> ")"
  show (VRight a) = "VRight " <> show a

instance (Eq a, Eq (Vary errs)) => Eq (VEither errs a) where
  a == b = toVary a == toVary b

instance (Ord a, Ord (Vary errs)) => Ord (VEither errs a) where
  compare a b = compare (toVary a) (toVary b)

instance (NFData a, NFData (Vary errs)) => NFData (VEither errs a) where
  rnf = toVary >>> rnf

instance Functor (VEither errs) where
  fmap :: forall a b. (a -> b) -> VEither errs a -> VEither errs b
  {-# INLINE fmap #-}
  fmap = mapRight

instance Applicative (VEither errs) where
  {-# INLINE pure #-}
  pure = VRight

  {-# INLINE (<*>) #-}
  (VRight fun) <*> (VRight val) = VRight (fun val)
  (VLeft err) <*> _ = (VLeft err)
  _ <*> (VLeft err) = (VLeft err)

instance Monad (VEither errs) where
  (>>=) :: forall a b. VEither errs a -> (a -> VEither errs b) -> VEither errs b
  (VRight a) >>= fun = fun a
  (VLeft err) >>= _  = (VLeft err)

instance Foldable (VEither errs) where
    foldMap _ (VLeft _) = mempty
    foldMap f (VRight y) = f y

    foldr _ z (VLeft _) = z
    foldr f z (VRight y) = f y z

    length (VLeft _)  = 0
    length (VRight _) = 1

instance Traversable (VEither errs) where
    traverse _ (VLeft x) = pure (VLeft x)
    traverse f (VRight y) = VRight <$> f y

instance Semigroup (VEither errs a) where
  (VRight a) <> _ = (VRight a)
  _ <> b = b

-- Conceptually VEither is a Bifunctor,
-- but the kind does not align :-(
-- p has to be Type -> Type -> Type
-- But in the case of VEither it is [Type] -> Type -> Type
--
-- instance Bifunctor VEither where
--   first = mapLeft
--   second = mapRight
--   bimap = veither

-- Look ma! A Hand-written Generic instance!
instance Generic (VEither errs a) where
  type (Rep (VEither errs a)) =  D1
       (MetaData "VEither" "Vary.VEither" "vary" False)
       (C1
          (MetaCons "VLeft" PrefixI False)
          (S1
             (MetaSel
                Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
             (Rec0 (Vary errs)))
        :+: C1
              (MetaCons "VRight" PrefixI False)
              (S1
                 (MetaSel
                    Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                 (Rec0 a)))
  from :: VEither errs a -> Rep (VEither errs a) x
  from = toEither >>> from >>> unsafeCoerce
  to :: Rep (VEither errs a) x -> VEither errs a 
  to = unsafeCoerce >>> to >>> fromEither
