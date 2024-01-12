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
{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Vary.Utils(
  (:|), KnownPrefix(..), Length, Subset(..), Index, IndexOf, Mappable, 
  
  natValue, 

    -- * Informational
    size,
    activeIndex,
) where
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

-- | Constrain `es` to be any type list containing `e`.
--
-- Useful to talk about variants generically without having to specify the exact type list right away.
--
-- For instance, the type of `Vary.from` is
--
-- > Vary.from :: (a :| l) => a -> Vary l
--
-- because we can use it to construct /any/ Vary as long as there is an @a@ somewhere in its list of types.
type (:|) e es = Member e es

-- | Returns the number of elements contained in this variant.
--
-- Does not actually use the runtime representation of the variant in any way.
size :: forall xs. (KnownNat (Length xs)) => Vary xs -> Word
size _ = natValue @(Length xs)

-- | Returns the currently active 'tag index' of the variant.
--
-- Not useful in normal code, but maybe nice in certaing debugging scenarios.
--
-- Note that this index changes whenever a variant is `Vary.morph`ed.
activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx



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
type Mappable a b xs ys = (a :| xs, b :| ys, ys ~ Mapped a b xs) -- , xs ~ Mapped b a ys)

-- | Compute a HList where the type a was changed into b.
type family Mapped (a :: Type) (b :: Type) (as :: [Type]) = (bs :: [Type]) where
  Mapped a b (a ': as)  = (b ': as)
  Mapped a b (x ': as) = x ': (Mapped a b as)
  Mapped a b l = TypeError (
    'Text "Cannot map from " ':<>: 'ShowType a ':<>: 'Text " into " ':<>: 'ShowType b 
    :$$: 'Text "as it cannot be found in the list " ':<>: 'ShowType l)

type family Mapped2 (a :: Type) (b :: Type) (as :: [Type]) = (bs :: [Type]) where
  Mapped2 a b as = ReplacedAt (IndexOf a as) b as

type family ReplacedAt (n :: Nat) (t :: Type) (xs :: [Type]) = (bs :: [Type]) where
  ReplacedAt 0 t (x ': xs) = (t ': xs)
  ReplacedAt n t (x ': xs) = (x : (ReplacedAt (n-1) t xs))

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
type Index (n :: Nat) (l :: [k]) = Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location n l l

-- We use this ridiculous name
-- to make it clear to the user when they see it in a type error
-- how to resolve that type error.
type family Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location (n :: Nat) (l :: [k]) (l2 :: [k]) :: k where
   Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location 0 (x ': _ ) _  = x
   Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location n (_ ': xs) l2 = Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location (n-1) xs l2
   Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location n '[]       l2 = TypeError ( 'Text "Index "
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
