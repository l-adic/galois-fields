module Data.Field.Galois.Base
  ( module Data.Field.Galois.Base,
  )
where

import Control.Monad.Random (Random)
import Data.Field (Field)
import Data.Group qualified as G (Group (..))
import GHC.Natural (Natural)
import Protolude hiding (one, quot, (-))
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Galois fields @GF(p^q)@ for @p@ prime and @q@ non-negative.
class
  ( Arbitrary k,
    Field k,
    Fractional k,
    Generic k,
    G.Group k,
    Ord k,
    Pretty k,
    Random k,
    Show k,
    NFData k
  ) =>
  GaloisField k
  where
  {-# MINIMAL char, deg, frob #-}

  -- | Characteristic @p@ of field and order of prime subfield.
  char :: k -> Natural

  -- | Degree @q@ of field as extension field over prime subfield.
  deg :: k -> Word

  -- | Frobenius endomorphism @x -> x^p@ of prime subfield.
  frob :: k -> k

  -- | Order @p^q@ of field.
  order :: k -> Natural
  order = (^) <$> char <*> deg
  {-# INLINEABLE order #-}

-- | Exponentiation of field element to integer.
pow :: (GaloisField k, Integral n) => k -> n -> k
pow = G.pow
{-# INLINEABLE pow #-}
{-# SPECIALIZE pow ::
  (GaloisField k) => k -> Int -> k,
  (GaloisField k) => k -> Integer -> k,
  (GaloisField k) => k -> Natural -> k,
  (GaloisField k) => k -> Word -> k
  #-}
