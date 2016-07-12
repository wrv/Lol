
module TensorBenches (tensorBenches) where

import Crypto.Lol.Factored
import Crypto.Lol.Cyclotomic.Tensor.CTensor    as X

import Criterion

import Data.Int

tensorBenches :: Benchmark
tensorBenches =
  let x = CT $ scalarPow' 0 :: CT F128 Int64
  in bench "Tensor.l" $ nf (wrap ctl) x
