{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             RankNTypes, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This sub-module exists only because we can't define and use
-- template Haskell splices in the same module.

module Crypto.Lol.FactoredDefs where

import Crypto.Lol.PosBinDefs

import Control.Arrow
import Data.Tagged
import Data.Singletons.Prelude   hiding ((:-))
import Data.Singletons.TH

singletons [d|
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

-- | Value-level prime-power factorization tagged by a 'Factored' type.
ppsFact :: forall m . Fact m => Tagged m [PP]
ppsFact = Tagged $ map ppToPP $ unF $ fromSing (sing :: SFactored m)

totientFact :: Fact m => Tagged m Int
totientFact = totientPPs <$> ppsFact

-- | Conversion.
ppToPP :: PrimePower -> PP
ppToPP = (binToInt . unP *** posToInt) . unPP

-- | Totient of a prime power.
totientPP :: PP -> Int
totientPP (_,0) = 1
totientPP (p,e) = (p-1)*(p^(e-1))

totientPPs :: [PP] -> Int
-- | Product of totients of individual 'PP's
totientPPs = product . map totientPP
