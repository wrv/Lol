
module UCycBenches (ucycBenches) where

import Crypto.Lol.FactoredDefs
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Criterion

ucycBenches :: Benchmark
ucycBenches =
  let x = Dec $ CT $ scalarPow' 0 :: UCyc F128
  in bench "UCyc.toPow" $ nf toPow x
