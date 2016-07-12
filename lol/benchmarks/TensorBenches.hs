
module TensorBenches (tensorBenches) where

import Crypto.Lol.FactoredDefs
import Crypto.Lol.Cyclotomic.Tensor.CTensor    as X

import Criterion

tensorBenches :: Benchmark
tensorBenches =
  let x = CT $ scalarPow' 0 :: CT F128
  in bench "Tensor.l" $ nf l x
