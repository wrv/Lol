{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types

import Criterion
import Control.Monad.Random

ucycBenches :: Benchmark
ucycBenches =
  let x = zero :: UCyc RT F128 D (ZqBasic 257 Int64)
      x' = zero :: UCyc CT F128 D (ZqBasic 257 Int64)
  in bgroup "UCyc" [
      bench "RT.l"   $ nf toPow x,
      bench "CT.l"   $ nf toPow x'
     ]
