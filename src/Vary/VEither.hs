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
{-# LANGUAGE InstanceSigs #-}
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

module Vary.VEither where

import Data.Function ((&))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)
import Vary

newtype VEither (errs :: [Type]) a = VEither (Vary (a ': errs))

instance Functor (VEither errs) where
  fmap :: forall a b. (a -> b) -> VEither errs a -> VEither errs b
  fmap fun (VEither vary) =
    vary
      & on @a (Vary.from . fun) Vary.morph
      & VEither

instance Applicative (VEither errs) where
  pure :: forall a. a -> VEither errs a
  pure = VEither . Vary.from
  (<*>) :: forall a b. VEither errs (a -> b) -> VEither errs a -> VEither errs b
  (VEither vfun) <*> (VEither vval) =
    vfun
      & on @(a -> b) inner Vary.morph
      & VEither
    where
      inner fun = vval & on @a (Vary.from @b . fun) Vary.morph

instance Monad (VEither errs) where
  (>>=) :: forall a b. VEither errs a -> (a -> VEither errs b) -> VEither errs b
  (VEither val) >>= fun = 
    val & on @a fun (VEither . Vary.morph)
