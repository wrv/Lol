
module UCycBenches (ucycBenches) where

import FactoredDefs
import CTensor
import Criterion

ucycBenches :: Benchmark
ucycBenches =
  let x = Dec $ CT $ scalarPow' 0 :: UCyc F128
  in bench "UCyc.toPow" $ nf toPow x
