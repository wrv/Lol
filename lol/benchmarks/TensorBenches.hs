
module TensorBenches (tensorBenches) where

import Crypto.Lol.Factored
import Crypto.Lol.Types.Numeric
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor    as X

import Criterion

tensorBenches :: Benchmark
tensorBenches =
  let x = scalarPow zero :: CT F128 Int64
  in bench "Tensor.l" $ nf l x
