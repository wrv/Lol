{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, InstanceSigs,
             KindSignatures, PolyKinds, RankNTypes,
             ScopedTypeVariables, TemplateHaskell,
             TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This sub-module exists only because we can't define and use
-- template Haskell splices in the same module.

module Crypto.Lol.PosBinDefs where

import Data.Singletons.Prelude
import Data.Singletons.TH
singletons [d|
            -- Positive naturals (1, 2, ...) in Peano representation.
            data Pos = O     -- one
                     | S Pos -- successor

           |]

-- not promotable due to numeric output

-- | Convert a 'Pos' to an integral type.
posToInt :: Integral z => Pos -> z
posToInt O = 1
posToInt (S a) = 1 + posToInt a

singletons [d|
            -- Positive naturals in binary representation.
            data Bin = B1       -- 1
                     | D0 Bin   -- 2*b (double)
                     | D1 Bin   -- 1 + 2*b (double and increment)

           |]

-- | Convert a 'Bin' to an integral type.
binToInt :: Integral z => Bin -> z
binToInt B1 = 1
binToInt (D0 a) = 2 * binToInt a
binToInt (D1 a) = 1 + 2 * binToInt a