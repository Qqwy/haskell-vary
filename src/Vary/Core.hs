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

module Vary.Core (Vary (..), pop) where

import Data.Kind (Type)
import GHC.Exts (Any)
import qualified Unsafe.Coerce as Data.Coerce
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception(..))
import Data.Typeable (Typeable, typeOf)

-- $setup
-- >>> :set -XGHC2021
-- >>> :set -XDataKinds

-- | Vary, contains one value out of a set of possibilities
--
-- Vary is what is known as a /Variant/ type.
-- This is also known as an /open union/ or /coproduct/, among other names.
--
-- You can see it as the generalization of `Either`.
-- Conceptually, these are the same:
--
-- > Vary [a, b, c, d, e]
-- > Either a (Either b (Either c (Either d e)))
--
-- However, compared to a deeply nested `Either`, `Vary` is:
--
-- - Much easier to work with;
-- - Much more efficient, as a single (strict) word is used for the tag.
--
-- `Vary`'s can be constructed with "Vary".`Vary.from` and values can be extracted using "Vary".`Vary.into` and "Vary".'Vary.on' .
data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any

emptyVaryError :: forall anything. String -> Vary '[] -> anything
emptyVaryError name = error (name <> " was called on empty Vary '[]")


-- | Attempts to extract a value of the first type from the `Vary`.
--
-- If this failed, we know it has to be one of the other possibilities.
--
-- This function can also be seen as turning one layer of `Vary` into its isomorphic `Either` representation.
--
-- This function is not often useful in 'normal' code, but /super/ useful in generic code where you want to recurse on the variant's types.
--
-- For instance when implementing a typeclass for any `Vary` whose elements implement the typeclass:
--
--
-- > instance Show (Vary '[]) where
-- >    show = Vary.exhaustiveCase
-- >
-- > instance (Show a, Show (Vary as)) => Show (Vary (a : as)) where
-- >    show vary = case Vary.pop vary of
-- >        Right val -> "Vary.from " <> show val
-- >        Left other -> show other
--
-- To go the other way: 
--
-- - Use "Vary".`Vary.morph` to turn @Vary as@ back into @Vary (a : as)@
-- - Use "Vary".`Vary.from` to turn @a@ back into @Vary (a : as)@
pop :: Vary (a : as) -> Either (Vary as) a
{-# INLINE pop #-}
pop (Vary 0 val) = Right (Data.Coerce.unsafeCoerce val)
pop (Vary tag val) = Left (Data.Coerce.unsafeCoerce (Vary (tag - 1) val))

instance Eq (Vary '[]) where
  (==) = emptyVaryError "Eq.(==)"

instance (Eq a, Eq (Vary as)) => Eq (Vary (a : as)) where
    {-# INLINE (==) #-}
    a == b = pop a == pop b

instance Ord (Vary '[]) where
    compare = emptyVaryError "Ord.compare"

instance (Ord a, Ord (Vary as)) => Ord (Vary (a : as)) where
    {-# INLINE compare #-}
    l `compare` r = pop l `compare` pop r

instance Show (Vary '[]) where
    show = emptyVaryError "Show.show"

-- | `Vary`'s 'Show' instance only works for types which are 'Typeable'
--
-- This allows us to print the name of the type which
-- the current value is of.
--
-- >>> Vary.from @Bool True :: Vary '[Int, Bool, String]
-- Vary.from @Bool True
--
-- >>> Vary.from @(Maybe Int) (Just 1234) :: Vary '[Maybe Int, Bool]
-- Vary.from @(Maybe Int) (Just 1234)
instance (Typeable a, Show a, Show (Vary as)) => Show (Vary (a : as)) where
    showsPrec d vary = case pop vary of
        Right val ->
            showString "Vary.from " . 
            showString "@" . 
            showsPrec (d+10) (typeOf val) . 
            showString " " . 
            showsPrec (d+11) val
        Left other -> showsPrec d other

instance NFData (Vary '[]) where
    rnf = emptyVaryError "NFData.rnf"

instance (NFData a, NFData (Vary as)) => NFData (Vary (a : as)) where
    {-# INLINE rnf #-}
    rnf vary = rnf (pop vary)


instance (Typeable (Vary '[]), Show (Vary '[])) => Exception (Vary '[]) where

-- | See [Vary and Exceptions](#vary_and_exceptions) for more info.
instance (Exception e, Exception (Vary errs), Typeable errs) => Exception (Vary (e : errs)) where
    displayException vary = 
        case pop vary of
            Right val -> displayException val
            Left rest -> displayException rest

    toException vary = 
        case pop vary of
            Right val -> toException val
            Left rest -> toException rest
    
    fromException some_exception = 
        case fromException @e some_exception of
            Just e -> Just (Vary 0 (Data.Coerce.unsafeCoerce e))
            Nothing -> case fromException @(Vary errs) some_exception of
                Just (Vary tag err) -> Just (Data.Coerce.unsafeCoerce (Vary (tag+1) err))
                Nothing -> Nothing
