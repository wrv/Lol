{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Factored where

import Control.Arrow
import Data.Tagged
import Data.Singletons.Prelude   hiding ((:-))
import Data.Singletons.TH

singletons [d|
            -- Positive naturals (1, 2, ...) in Peano representation.
            data Pos = O     -- one
                     | S Pos -- successor

            data Bin = B1       -- 1
                     | D0 Bin   -- 2*b (double)
                     | D1 Bin   -- 1 + 2*b (double and increment)

            -- restrict to primes
            newtype PrimeBin = P Bin

            -- (prime, exponent)
            newtype PrimePower = PP (PrimeBin,Pos)

            -- Invariant: primes appear in strictly increasing
            -- order (no duplicates).
            newtype Factored = F [PrimePower]

            -- Unwrap 'PrimeBin'
            unP :: PrimeBin -> Bin
            unP (P p) = p

            -- Unwrap 'PrimePower'.
            unPP :: PrimePower -> (PrimeBin,Pos)
            unPP (PP pp) = pp

            -- Unwrap 'Factored'
            unF :: Factored -> [PrimePower]
            unF (F pps) = pps

            f128 :: Factored
            f128 = F [PP (P $ D0 B1, S $ S $ S $ S $ S $ S O)]

            |]

type Fact (m :: Factored) = SingI m

-- | Type synonym for @(prime, exponent)@ pair.
type PP = (Int, Int)

-- | Totient of a Factored type
--{-# INLINABLE totientFact #-}
totientFact :: Fact m => Tagged m Int
totientFact = totientPPs <$> ppsFact

-- | Product of totients of individual 'PP's
--{-# INLINABLE totientPPs #-}
totientPPs :: [PP] -> Int
totientPPs = product . map totientPP

-- | Totient of a prime power.
--{-# INLINABLE totientPP #-}
totientPP :: PP -> Int
totientPP (_,0) = 1
totientPP (p,e) = (p-1)*(p^(e-1))

-- | Value-level prime-power factorization tagged by a 'Factored' type.
--{-# INLINABLE ppsFact #-}
ppsFact :: forall m . Fact m => Tagged m [PP]
ppsFact = Tagged $ map ppToPP $ unF $ fromSing (sing :: SFactored m)

-- | Conversion.
--{-# INLINABLE ppToPP #-}
ppToPP :: PrimePower -> PP
ppToPP = (binToInt . unP *** posToInt) . unPP

-- | Convert a 'Pos' to an integral type.
--{-# INLINABLE posToInt #-}
posToInt :: Integral z => Pos -> z
posToInt O = 1
posToInt (S a) = 1 + posToInt a

-- | Convert a 'Bin' to an integral type.
--{-# INLINABLE binToInt #-}
binToInt :: Integral z => Bin -> z
binToInt B1 = 1
binToInt (D0 a) = 2 * binToInt a
binToInt (D1 a) = 1 + 2 * binToInt a
