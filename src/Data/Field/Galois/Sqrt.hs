module Data.Field.Galois.Sqrt
  ( qnr,
    qr,
    quad,
    rnd,
    rndR,
    sr,
  )
where

import Control.Monad.Random (MonadRandom, StdGen, getRandom, getRandomR, mkStdGen, runRand)
import Data.Field.Galois.Base (GaloisField (..), pow)
import GHC.Natural (Natural)
import Protolude

-------------------------------------------------------------------------------
-- Main functions
-------------------------------------------------------------------------------

-- | Get randomised quadratic nonresidue.
qnr :: (GaloisField k) => Maybe k
qnr = getQNR
{-# INLINEABLE qnr #-}

-- | Check if quadratic residue.
qr :: (GaloisField k) => k -> Bool
qr = not . isQNR
{-# INLINEABLE qr #-}

-- | Solve quadratic @ax^2 + bx + c = 0@ over field.
quad :: (GaloisField k) => k -> k -> k -> Maybe k
quad = solveQuadratic
{-# INLINEABLE quad #-}

-- | Randomised field element.
rnd :: (GaloisField k, MonadRandom m) => m k
rnd = getRandom
{-# INLINEABLE rnd #-}

-- | Randomised field element in range.
rndR :: (GaloisField k, MonadRandom m) => (k, k) -> m k
rndR = getRandomR
{-# INLINEABLE rndR #-}

-- | Square root of field element.
sr :: (GaloisField k) => k -> Maybe k
sr = squareRoot
{-# INLINEABLE sr #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Check if an element is a quadratic nonresidue.
isQNR :: (GaloisField k) => k -> Bool
isQNR n = n == 0 || char n /= 2 && pow n (shiftR (order n) 1) /= 1
{-# INLINEABLE isQNR #-}

-- Get a random quadratic nonresidue.
getQNR :: forall k. (GaloisField k) => Maybe k
getQNR
  | char (witness :: k) == 2 = Nothing
  | otherwise = Just $ getQNR' $ runRand rnd $ mkStdGen 0
  where
    getQNR' :: (k, StdGen) -> k
    getQNR' (x, g)
      | x /= 0 && isQNR x = x
      | otherwise = getQNR' $ runRand rnd g
{-# INLINEABLE getQNR #-}

-- Factor the order @p - 1@ to get @q@ and @s@ such that @p - 1 = q2^s@.
factorOrder :: (GaloisField k) => k -> (Natural, Word)
factorOrder w = factorOrder' (order w - 1, 0)
  where
    factorOrder' :: (Natural, Word) -> (Natural, Word)
    factorOrder' qs@(q, s)
      | testBit q 0 = qs
      | otherwise = factorOrder' (shiftR q 1, s + 1)
{-# INLINEABLE factorOrder #-}

-- Get a square root of @n@ with the Tonelli-Shanks algorithm.
squareRoot :: forall k. (GaloisField k) => k -> Maybe k
squareRoot 0 = Just 0
squareRoot n
  | char n == 2 = Just $ power n
  | isQNR n = Nothing
  | otherwise = case (factorOrder n, getQNR) of
      ((q, s), Just z) ->
        let zq = pow z q
            nq = pow n $ shiftR q 1
            nnq = n * nq
         in loop s zq (nq * nnq) nnq
      _ -> panic "Sqrt.squareRoot: no quadratic nonresidue."
  where
    power :: k -> k
    power = next $ deg n
      where
        next :: Word -> k -> k
        next 1 m = m
        next i m = next (i - 1) (m * m)
    loop :: Word -> k -> k -> k -> Maybe k
    loop _ _ 0 _ = Just 0
    loop _ _ 1 r = Just r
    loop m c t r =
      let i = least t 0
          b = pow c $ (bit (fromIntegral $ m - i - 1) :: Int)
          b2 = b * b
       in loop i b2 (t * b2) (r * b)
      where
        least :: k -> Word -> Word
        least 1 j = j
        least ti j = least (ti * ti) (j + 1)
{-# INLINEABLE squareRoot #-}

-- Solve a quadratic equation @ax^2 + bx + c = 0@.
solveQuadratic :: forall k. (GaloisField k) => k -> k -> k -> Maybe k
solveQuadratic 0 _ _ = Nothing
solveQuadratic _ _ 0 = Just 0
solveQuadratic a 0 c = squareRoot $ -c / a
solveQuadratic a b c
  | char a == 2 = (<$>) (* (b / a)) $ solveQuadratic' $ ac / bb
  | otherwise = (<$>) ((/ (2 * a)) . subtract b) $ squareRoot $ bb - 4 * ac
  where
    ac = a * c
    bb = b * b
    solveQuadratic' :: k -> Maybe k
    solveQuadratic' x
      | sum xs /= 0 = Nothing
      | odd m = Just $ sum h
      | otherwise = panic "Base.solveQuadratic: to be implemented."
      where
        m = deg x
        xs = take (fromIntegral m) $ iterate (join (*)) x
        h = zipWith ($) (cycle [identity, const 0]) xs
{-# INLINEABLE solveQuadratic #-}
