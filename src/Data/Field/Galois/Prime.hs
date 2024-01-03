{-# LANGUAGE DerivingStrategies #-}

module Data.Field.Galois.Prime
  ( Prime,
    PrimeField,
    fromP,
    toP,
  )
where

import Control.Monad.Random (Random (..))
import Data.Euclidean as S (Euclidean (..), GcdDomain (..))
import Data.Field (Field)
import Data.Field.Galois.Base (GaloisField (..))
import Data.Group (Group (..))
import Data.Mod (Mod, unMod, (^%))
import Data.Semiring (Ring (..), Semiring (..))
import GHC.Natural (naturalToInteger)
import GHC.TypeNats (natVal)
import Protolude as P hiding (Semiring, natVal, rem)
import Test.QuickCheck (Arbitrary (..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty (..))

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p) = Z/pZ@ for @p@ prime.
class (GaloisField k) => PrimeField k where
  {-# MINIMAL fromP #-}

  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
data Prime (p :: Nat) = P (Mod p)
  deriving (Eq, Ord, Show, Generic)

instance (KnownNat p) => Num (Prime p) where
  (P x) + (P y) = P (x + y)
  (P x) * (P y) = P (x * y)
  abs (P x) = P (abs x)
  signum (P x) = P (signum x)
  fromInteger n = P (fromInteger n)
  negate (P x) = P (P.negate x)

instance (KnownNat p) => Fractional (Prime p) where
  recip (P x) = P (recip x)
  fromRational n = P (fromRational n)

instance (KnownNat p) => Semiring (Prime p) where
  (P x) `times` (P y) = P (x * y)
  one = P 1
  plus (P x) (P y) = P (x + y)
  zero = P 0

instance (KnownNat p) => Ring (Prime p) where
  negate (P x) = P (P.negate x)

instance (KnownNat p) => GcdDomain (Prime p) where
  divide (P x) (P y) = Just $ P (x / y)
  gcd (P x) (P y) = P (S.gcd x y)
  lcm (P x) (P y) = P (S.lcm x y)

-- Num, Fractional, Euclidean, Field, GcdDomain, Ring, Semiring, Bounded, Enum, NFData)

instance Hashable (Prime p) where
  hashWithSalt s (P x) = hashWithSalt s (unMod x)

-- Prime fields are convertible.
instance (KnownNat p) => PrimeField (Prime p) where
  fromP (P x) = naturalToInteger (unMod x)
  {-# INLINEABLE fromP #-}

instance (KnownNat p) => S.Euclidean (Prime p) where
  quotRem (P x) (P y) = (P q, P r)
    where
      (q, r) = S.quotRem x y
  {-# INLINEABLE quotRem #-}
  degree = const 1
  {-# INLINEABLE degree #-}

instance (KnownNat p) => Field (Prime p)

instance NFData (Prime p)

instance (KnownNat p) => Bounded (Prime p) where
  minBound = P 0
  {-# INLINEABLE minBound #-}
  maxBound = P (fromIntegral (natVal (Proxy :: Proxy p)) - 1)
  {-# INLINEABLE maxBound #-}

instance (KnownNat p) => Enum (Prime p) where
  toEnum = fromIntegral
  {-# INLINEABLE toEnum #-}
  fromEnum = fromInteger . fromP
  {-# INLINEABLE fromEnum #-}

-- Prime fields are Galois fields.
instance (KnownNat p) => GaloisField (Prime p) where
  char = natVal
  {-# INLINEABLE char #-}
  deg = const 1
  {-# INLINEABLE deg #-}
  frob = identity
  {-# INLINEABLE frob #-}

{-# RULES
"Prime.pow" forall (k :: (KnownNat p) => Prime p) n.
  (^) k n =
    pow k n
  #-}

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

-- Prime fields are multiplicative groups.
instance (KnownNat p) => Group (Prime p) where
  invert = recip
  {-# INLINE invert #-}
  pow (P x) k = P (x ^% k)
  {-# INLINE pow #-}

-- Prime fields are multiplicative monoids.
instance (KnownNat p) => Monoid (Prime p) where
  mempty = P 1
  {-# INLINE mempty #-}

-- Prime fields are multiplicative semigroups.
instance (KnownNat p) => Semigroup (Prime p) where
  (<>) = (*)
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance (KnownNat p) => Arbitrary (Prime p) where
  arbitrary = choose (minBound, maxBound)
  {-# INLINEABLE arbitrary #-}

-- Prime fields are integral.
instance (KnownNat p) => Integral (Prime p) where
  quotRem = S.quotRem
  {-# INLINE quotRem #-}
  toInteger = fromP
  {-# INLINEABLE toInteger #-}

-- Prime fields are pretty.
instance (KnownNat p) => Pretty (Prime p) where
  pretty (P x) = pretty $ naturalToInteger $ unMod x

-- Prime fields are random.
instance (KnownNat p) => Random (Prime p) where
  random = randomR (minBound, maxBound)
  {-# INLINEABLE random #-}
  randomR (a, b) = first fromInteger . randomR (fromP a, fromP b)
  {-# INLINEABLE randomR #-}

-- Prime fields are real.
instance (KnownNat p) => Real (Prime p) where
  toRational = fromIntegral
  {-# INLINEABLE toRational #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(p)@.
toP :: (KnownNat p) => Integer -> Prime p
toP = fromInteger
{-# INLINEABLE toP #-}
