
module TensorBenches (tensorBenches) where

import FactoredDefs
import CTensor

import Criterion

tensorBenches :: Benchmark
tensorBenches =
  let x = CT $ scalarPow' 0 :: CT F128
  in bench "Tensor.l" $ nf l x
