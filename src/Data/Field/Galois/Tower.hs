{-# LANGUAGE UndecidableInstances #-}

module Data.Field.Galois.Tower
  ( (*^)
  ) where

import Protolude

import Data.Field.Galois.Base (GaloisField)
import Data.Field.Galois.Prime (Prime, fromP)
import Data.Field.Galois.Extension (Extension, IrreducibleMonic, toE')
import Data.Field.Galois.Binary (Binary, toB')

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Tower of fields @L@ over @K@ strict partial ordering.
class (GaloisField k, GaloisField l) => TowerOfFields k l where
  {-# MINIMAL embed #-}
  -- | Embed @K@ into @L@ naturally.
  embed :: k -> l

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Extension fields are towers of fields.
instance {-# OVERLAPPING #-}
  IrreducibleMonic k im => TowerOfFields k (Extension k im) where
  embed = toE' . return
  {-# INLINABLE embed #-}

-- Extension field towers are transitive.
instance {-# OVERLAPPABLE #-}
  (TowerOfFields k l, IrreducibleMonic l im, TowerOfFields l (Extension l im))
  => TowerOfFields k (Extension l im) where
  embed = embed . (embed :: k -> l)
  {-# INLINABLE embed #-}

-- Binary fields are towers of fields.
instance KnownNat im => TowerOfFields (Prime 2) (Binary im) where
  embed = toB' . fromP
  {-# INLINABLE embed #-}

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Scalar multiplication.
infixl 7 *^
(*^) :: TowerOfFields k l => k -> l -> l
(*^) = (*) . embed
{-# INLINE (*^) #-}
