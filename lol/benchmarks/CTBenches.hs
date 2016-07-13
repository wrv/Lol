
{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module CTBenches (ctBenches) where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Criterion
import Utils
import Control.Monad.Random

ctBenches :: IO Benchmark
ctBenches = do
  x :: CT F128 (Zq 257) <- getRandom
  return $ bench "CT.l" $ nf l' x
