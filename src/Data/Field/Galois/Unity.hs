{-# LANGUAGE UndecidableInstances #-}

module Data.Field.Galois.Unity
  ( CyclicSubgroup (..),
    RootsOfUnity,
    cardinality,
    cofactor,
    isPrimitiveRootOfUnity,
    isRootOfUnity,
    toU,
    toU',
    fromU,
  )
where

import Control.Monad.Random (Random (..))
import Data.Field.Galois.Base (GaloisField (..))
import Data.Field.Galois.Prime (Prime)
import Data.Group (Group (..))
import GHC.Natural (Natural, naturalToInteger)
import GHC.TypeNats (natVal)
import Protolude hiding (natVal)
import Test.QuickCheck (Arbitrary (..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty (..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Cyclic subgroups of finite groups.
class (Group g) => CyclicSubgroup g where
  {-# MINIMAL gen #-}

  -- | Generator of subgroup.
  gen :: g

-- | @n@-th roots of unity of Galois fields.
newtype RootsOfUnity (n :: Nat) k = U k
  deriving (Bits, Eq, Functor, Generic, NFData, Ord, Show)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Roots of unity cyclic subgroups are arbitrary.
instance
  ( KnownNat n,
    GaloisField k,
    CyclicSubgroup (RootsOfUnity n k)
  ) =>
  Arbitrary (RootsOfUnity n k)
  where
  arbitrary = pow gen <$> choose (0, naturalToInteger $ order (witness :: Prime n) - 1)
  {-# INLINEABLE arbitrary #-}

-- Roots of unity are groups.
instance (KnownNat n, GaloisField k) => Group (RootsOfUnity n k) where
  invert (U x) = U $ recip x
  {-# INLINEABLE invert #-}
  pow (U x) n = U $ pow x n
  {-# INLINEABLE pow #-}

-- Roots of unity are monoids.
instance (KnownNat n, GaloisField k) => Monoid (RootsOfUnity n k) where
  mempty = U 1
  {-# INLINEABLE mempty #-}

-- Roots of unity are pretty.
instance (KnownNat n, GaloisField k) => Pretty (RootsOfUnity n k) where
  pretty (U x) = pretty x

-- Roots of unity cyclic subgroups are random.
instance
  ( KnownNat n,
    GaloisField k,
    CyclicSubgroup (RootsOfUnity n k)
  ) =>
  Random (RootsOfUnity n k)
  where
  random = first (pow gen) . randomR (0, naturalToInteger $ order (witness :: Prime n) - 1)
  {-# INLINEABLE random #-}
  randomR = panic "Unity.randomR: not implemented."

-- Roots of unity are semigroups.
instance (KnownNat n, GaloisField k) => Semigroup (RootsOfUnity n k) where
  U x <> U y = U $ x * y
  {-# INLINEABLE (<>) #-}

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Cardinality of subgroup.
cardinality :: forall n k. (KnownNat n) => RootsOfUnity n k -> Natural
cardinality = const $ natVal (witness :: Prime n)
{-# INLINEABLE cardinality #-}

-- | Cofactor of subgroup in group.
cofactor :: forall n k. (KnownNat n, GaloisField k) => RootsOfUnity n k -> Natural
cofactor = quot (order (witness :: k)) . cardinality
{-# INLINEABLE cofactor #-}

-- | Check if element is primitive root of unity.
isPrimitiveRootOfUnity :: (KnownNat n, GaloisField k) => RootsOfUnity n k -> Bool
isPrimitiveRootOfUnity u@(U x) =
  isRootOfUnity u
    && not (any (isUnity x) ([1 .. cardinality u - 1] :: [Natural]))
{-# INLINEABLE isPrimitiveRootOfUnity #-}

-- | Check if element is root of unity.
isRootOfUnity :: (KnownNat n, GaloisField k) => RootsOfUnity n k -> Bool
isRootOfUnity u@(U x) = isUnity x $ cardinality u
{-# INLINEABLE isRootOfUnity #-}

-- | Check if element is unity.
isUnity :: (Integral n, GaloisField k) => k -> n -> Bool
isUnity = ((==) 1 .) . pow
{-# INLINEABLE isUnity #-}

-- | Safe convert from field to roots of unity.
toU :: forall n k. (KnownNat n, GaloisField k) => k -> RootsOfUnity n k
toU x =
  let u = U x :: RootsOfUnity n k
   in if isRootOfUnity u then u else panic "Unity.toUnity: element is not a root of unity."
{-# INLINEABLE toU #-}

-- | Unsafe convert from field to roots of unity.
toU' :: forall n k. k -> RootsOfUnity n k
toU' = U
{-# INLINEABLE toU' #-}

fromU :: forall n k. RootsOfUnity n k -> k
fromU (U k) = k
{-# INLINEABLE fromU #-}
