{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module TensorBenches (tensorBenches) where

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor    as X
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor as X
import Crypto.Lol.Types.ZqBasic                as X

import Criterion

tensorBenches :: Benchmark
tensorBenches =
  let x = scalarPow zero :: RT F128 (ZqBasic 257 Int64)
      y = scalarPow zero :: CT F128 (ZqBasic 257 Int64)
  in bgroup "Tensor" [
      bench "RT.l" $ nf l x,
      bench "CT.l" $ nf l y
     ]
