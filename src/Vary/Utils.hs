{-# OPTIONS_GHC -ddump-stg-from-core #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Vary.Utils where
import Vary.Core (Vary(..))

import Data.Kind ( Type, Constraint )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits
    ( KnownNat,
      TypeError,
      type (+),
      type (-),
      natVal,
      ErrorMessage(ShowType, (:$$:), (:<>:), Text),
      Nat )
import qualified Data.Vector.Unboxed as UVector
type UVector = UVector.Vector

type (:|) e es = Member e es


-- | Provide evidence that @xs@ is a subset of @es@.
class KnownPrefix es => Subset (xs :: [Type]) (es :: [Type]) where
  subsetFullyKnown :: Bool
  subsetFullyKnown =
    -- Don't show "minimal complete definition" in haddock.
    error "subsetFullyKnown"

  -- reifyIndices :: [Int]
  -- reifyIndices =
  --   -- Don't show "minimal complete definition" in haddock.
  --   error "reifyIndices"

  -- reifyIndicesVec :: UVector Int
  -- reifyIndicesVec = 
  --   -- Don't show "minimal complete definition" in haddock.
  --   error "reifyIndicesVec"

  
  morph' :: Vary xs -> Vary ys
  morph' = 
    -- Don't show "minimal complete definition" in haddock.
    -- Also, default for the empty instance :-)
    error "morph' was unexpectedly called"

-- If the subset is not fully known, make sure the subset and the base stack
-- have the same unknown suffix.
instance {-# INCOHERENT #-}
  ( KnownPrefix es
  , xs `IsUnknownSuffixOf` es
  ) => Subset xs es where
  subsetFullyKnown = False
  -- reifyIndices = []
  -- {-# INLINE reifyIndicesVec #-}
  -- reifyIndicesVec = UVector.empty


-- If the subset is fully known, we're done.
instance KnownPrefix es => Subset '[] es where
  subsetFullyKnown = True
  -- reifyIndices = []
  -- {-# INLINE reifyIndicesVec #-}
  -- reifyIndicesVec = UVector.empty -- UVector.empty

instance (e :| es, Subset xs es) => Subset (e : xs) es where
  subsetFullyKnown = subsetFullyKnown @xs @es
  -- reifyIndices = natValue @(IndexOf e es) : reifyIndices @xs @es
  -- {-# INLINE reifyIndicesVec #-}
  -- reifyIndicesVec = UVector.fromList (natValue @(IndexOf e es) : reifyIndices @xs @es) -- UVector.cons (natValue @(IndexOf e es)) (reifyIndicesVec @xs @es)

  morph' (Vary 0 a) = Vary (natValue @(IndexOf e es)) a
  morph' (Vary n a) = morph' @xs @es (Vary (n-1) a)


----

-- | Calculate length of a statically known prefix of @es@.
class KnownPrefix (es :: [Type]) where
  prefixLength :: Int

instance KnownPrefix es => KnownPrefix (e : es) where
  prefixLength = 1 + prefixLength @es

instance {-# INCOHERENT #-} KnownPrefix es where
  prefixLength = 0

----

-- | Require that @xs@ is the unknown suffix of @es@.
class (xs :: [k]) `IsUnknownSuffixOf` (es :: [k])
instance {-# INCOHERENT #-} xs ~ es => xs `IsUnknownSuffixOf` es
instance xs `IsUnknownSuffixOf` es => xs `IsUnknownSuffixOf` (e : es)

type family Hmm (xs :: [Type]) (ys :: [Type]) :: [Nat] where
  Hmm '[] _ = '[]
  Hmm _ '[] = '[] -- TODO
  Hmm (x ': xs) ys = (IndexOf x ys : Hmm xs ys)

-- | Get list length
type family Length (xs :: [k]) :: Nat where
   Length xs = Length' 0 xs

type family Length' n (xs :: [k]) :: Nat where
   Length' n '[]       = n
   Length' n (x ': xs) = Length' (n+1) xs

natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Constraint to link the input and output lists together.
--
-- By doing this, we can infer ys from (a,b,xs) or xs from (a,b,ys).
type Mappable a b xs ys = (a :| xs, b :| ys, ys ~ Mapped a b xs, xs ~ Mapped b a ys)

-- | Compute a HList where the type a was changed into b.
type family Mapped (a :: Type) (b :: Type) (as :: [Type]) = (bs :: [Type]) where
  Mapped a b (a ': as)  = (b ': as)
  Mapped a b (x ': as) = x ': (Mapped a b as)
  Mapped _ _ _ = TypeError ('Text "Boom") -- ( 'ShowType a ':<>: 'Text "not found in list" :$$: " " ':<>: 'ShowType l)

-- type family M2 (lhs :: (Type, Type, [Type])) = (rhs :: (Type, Type, [Type])) | rhs -> lhs where
--   M2 (a,b,(a ': as)) = (a,b, (b ': as))
--   M2 (a,b,(x ': as)) = (a,b, (x ': (M2 (a,b,as))))
--   M2 _ = TypeError ('Text "Boom")


-- | Get the first index of a type
type IndexOf (x :: k) (xs :: [k]) = IndexOf' (MaybeIndexOf x xs) x xs

-- | Get the first index of a type
type family IndexOf' (i :: Nat) (a :: k) (l :: [k]) :: Nat where
   IndexOf' 0 x l = TypeError ( 'ShowType x
                          ':<>: 'Text " not found in list:"
                          ':$$: 'Text " "
                          ':<>: 'ShowType l )
   IndexOf' i _ _ = i - 1

-- | Get the first index (starting from 1) of a type or 0 if none
type family MaybeIndexOf (a :: k) (l :: [k]) where
   MaybeIndexOf x xs = MaybeIndexOf' 0 x xs

-- | Helper for MaybeIndexOf
type family MaybeIndexOf' (n :: Nat) (a :: k) (l :: [k]) where
   MaybeIndexOf' n x '[]       = 0
   MaybeIndexOf' n x (x ': xs) = n + 1
   MaybeIndexOf' n x (y ': xs) = MaybeIndexOf' (n+1) x xs


-- | Indexed access into the list
type Index (n :: Nat) (l :: [k]) = Index' n l l

-- | Indexed access into the list
type family Index' (n :: Nat) (l :: [k]) (l2 :: [k]) :: k where
   Index' 0 (x ': _ ) _  = x
   Index' n (_ ': xs) l2 = Index' (n-1) xs l2
   Index' n '[]       l2 = TypeError ( 'Text "Index "
                                ':<>: 'ShowType n
                                ':<>: 'Text " out of bounds for list:"
                                ':$$: 'Text " "
                                ':<>: 'ShowType l2 )

-- | Constraint: x member of xs
type family Member x xs :: Constraint where
   Member x xs = MemberAtIndex (IndexOf x xs) x xs
   
type MemberAtIndex i x xs =
   ( x ~ Index i xs
   , KnownNat i
   )

-- | Remove (the first) `a` in `l`
type family Remove (a :: k) (l :: [k]) :: [k] where
   Remove a '[]       = '[]
   Remove a (a ': as) = as
   Remove a (b ': as) = b ': Remove a as
