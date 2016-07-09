{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module TensorBenches (tensorBenches) where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types

import Criterion
import Control.Monad.Random

tensorBenches :: Benchmark
tensorBenches =
  let x = zero :: RT F128 (ZqBasic 257 Int64)
      y = zero :: CT F128 (ZqBasic 257 Int64)
  in bgroup "Tensor" [
      bench "RT.l" $ nf l x,
      bench "CT.l" $ nf l y
     ]
