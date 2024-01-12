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
{-# LANGUAGE UndecidableInstances #-}
module Vary.Core (Vary(..)) where
import Data.Kind (Type)
import GHC.Exts (Any)
import Unsafe.Coerce

-- | Vary, contains one value out of a set of possibilities
--
-- Vary is what is known as a /Variant/ type.
-- This is also known as an /open union/ or /coproduct/, among other names.
--
-- You can see it as the generalization of `Either`.
-- Conceptually, these are the same:
--
-- > Vary '[a, b, c, d, e]
-- > Either a (Either b (Either c (Either d e)))
--
-- However, compared to a deeply nested `Either`, `Vary` is:
--
-- - Much easier to work with
-- - Much more efficient, as a single (strict) word is used for the tag.
--
-- `Vary`'s can be constructed with "Vary".`Vary.from` and values can be extracted using "Vary".`Vary.into` and "Vary".'Vary.on' .
data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any

instance Show (Vary '[]) where
    show = emptyVaryError "show"

instance (Show a, Show (Vary as)) => Show (Vary (a ': as)) where
    show (Vary 0 any) = "Vary.from " <> (show @a $ unsafeCoerce any)
    show (Vary n any) = show @(Vary as) $ unsafeCoerce (Vary (n-1) any)

emptyVaryError name = error (name <> " was called on empty Vary '[]")
