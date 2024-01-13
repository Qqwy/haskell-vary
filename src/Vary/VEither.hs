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
) where

import Control.Category ((>>>))
import Control.DeepSeq (NFData (..))
import qualified Data.Either
import Data.Kind (Type)
import Vary.Core (Vary(..))
import Vary ((:|))
import qualified Vary
import GHC.Generics
import Unsafe.Coerce

-- $setup
--
-- == General Usage
--
-- This module is intended to be used qualified:
--
-- >>> import Vary.VEither (VEither(VLeft, VRight))
-- >>> import qualified Vary.VEither
-- 
-- And for many functions, it is useful or outright necessary to enable the following extensions:
--
-- >>> :set -XGHC2021
-- >>> :set -XDataKinds

newtype VEither (errs :: [Type]) a = VEither (Vary (a : errs))

toVary :: VEither errs a -> Vary (a : errs)
{-# INLINE toVary #-}
toVary (VEither vary) = vary

fromVary :: Vary (a : errs) -> VEither errs a
{-# INLINE fromVary #-}
fromVary vary = VEither vary

toEither :: VEither errs a -> Either (Vary errs) a
{-# INLINE toEither #-}
toEither = toVary >>> Vary.pop

fromEither :: Either (Vary errs) a -> VEither errs a
{-# INLINE fromEither #-}
fromEither = Data.Either.either Vary.morph Vary.from >>> fromVary

-- | Shorthand to construct a VLeft from a single left value.
fromLeft :: forall err errs a. err :| errs => err -> VEither errs a
fromLeft = Vary.from @err >>> VLeft

fromRight :: forall a errs. a -> VEither errs a
fromRight = VRight

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

intoOnly :: forall a. VEither '[] a -> a
intoOnly (VRight a) = a
intoOnly (VLeft emptyVary) = Vary.exhaustiveCase emptyVary

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
  fmap fun ve = case ve of 
    VRight a -> VRight (fun a)
    VLeft errs -> VLeft errs

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
