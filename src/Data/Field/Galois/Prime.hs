{-# LANGUAGE StandaloneDeriving #-}

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
import Data.Modular (Mod, Modulus, unMod)
import Data.Semiring (Ring (..), Semiring (..))
import GHC.TypeNats (natVal)
import Protolude as P hiding (Semiring, natVal, rem)
import Test.QuickCheck (Arbitrary (..), choose)
import Text.PrettyPrint.Leijen.Text (Pretty (..))
import qualified Prelude

-------------------------------------------------------------------------------
-- Data types
-------------------------------------------------------------------------------

-- | Prime fields @GF(p) = Z/pZ@ for @p@ prime.
class (GaloisField k) => PrimeField k where
  {-# MINIMAL fromP #-}

  -- | Convert from @GF(p)@ to @Z@.
  fromP :: k -> Integer

-- | Prime field elements.
newtype Prime (p :: Nat) = P (Mod Integer p)
  deriving (Eq, Ord, Show, Generic)

deriving instance (Modulus p) => Num (Prime p)

deriving instance (Modulus p) => Fractional (Prime p)

instance (Modulus p) => Euclidean (Prime p) where
  degree = const 0
  quotRem x y = (x / y, 0)
  quot = (/)
  rem = const $ const 0
  {-# INLINE degree #-}

deriving instance (Modulus p) => Field (Prime p)

instance Hashable (Prime p) where
  hashWithSalt s (P x) = hashWithSalt s (unMod x)

instance (Modulus p) => GcdDomain (Prime p) where
  divide x y = Just (x / y)
  gcd = const $ const 1
  lcm = const $ const 1
  coprime = const $ const True

-- Prime fields are convertible.
instance (Modulus p) => PrimeField (Prime p) where
  fromP (P x) = unMod x
  {-# INLINEABLE fromP #-}

-- Prime fields are Galois fields.
instance (Modulus p) => GaloisField (Prime p) where
  char = natVal
  {-# INLINEABLE char #-}
  deg = const 1
  {-# INLINEABLE deg #-}
  frob = identity
  {-# INLINEABLE frob #-}

instance (Modulus p) => Ring (Prime p) where
  negate = Prelude.negate
  {-# INLINE negate #-}

instance (Modulus p) => Semiring (Prime p) where
  plus = (+)
  times = (*)
  zero = 0
  one = mx
    where
      mx = if natVal mx > 1 then 1 else 0
  fromNatural x = mx
    where
      mx = fromIntegral $ x `mod` natVal mx

deriving instance (Modulus p) => Bounded (Prime p)

deriving instance (Modulus p) => Enum (Prime p)

{-# RULES
"Prime.pow" forall (k :: (Modulus p) => Prime p) n.
  (^) k n =
    pow k n
  #-}

-------------------------------------------------------------------------------
-- Group instances
-------------------------------------------------------------------------------

-- Prime fields are multiplicative groups.
instance (Modulus p) => Group (Prime p) where
  invert = recip
  {-# INLINE invert #-}
  pow (P x) k = P (x ^ k)
  {-# INLINE pow #-}

-- Prime fields are multiplicative monoids.
instance (Modulus p) => Monoid (Prime p) where
  mempty = P 1
  {-# INLINE mempty #-}

-- Prime fields are multiplicative semigroups.
instance (Modulus p) => Semigroup (Prime p) where
  (<>) = (*)
  {-# INLINE (<>) #-}
  stimes = flip pow
  {-# INLINE stimes #-}

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

-- Prime fields are arbitrary.
instance (Modulus p) => Arbitrary (Prime p) where
  arbitrary = choose (minBound, maxBound)
  {-# INLINEABLE arbitrary #-}

-- Prime fields are integral.
instance (Modulus p) => Integral (Prime p) where
  quotRem = S.quotRem
  {-# INLINE quotRem #-}
  toInteger = fromP
  {-# INLINEABLE toInteger #-}

-- Prime fields are pretty.
instance (KnownNat p) => Pretty (Prime p) where
  pretty (P x) = pretty $ unMod x

-- Prime fields are random.
instance (Modulus p) => Random (Prime p) where
  random = randomR (minBound, maxBound)
  {-# INLINEABLE random #-}
  randomR (a, b) = first fromInteger . randomR (fromP a, fromP b)
  {-# INLINEABLE randomR #-}

-- Prime fields are real.
instance (Modulus p) => Real (Prime p) where
  toRational = fromIntegral
  {-# INLINEABLE toRational #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Safe convert from @Z@ to @GF(p)@.
toP :: (Modulus p) => Integer -> Prime p
toP = fromInteger
{-# INLINEABLE toP #-}
