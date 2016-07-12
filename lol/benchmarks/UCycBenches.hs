
module UCycBenches (ucycBenches) where

import Crypto.Lol.Factored
import Crypto.Lol.Types.Numeric
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Cyclotomic.Tensor.CTensor    as X
import Criterion

ucycBenches :: Benchmark
ucycBenches =
  let x = zero :: UCyc CT F128 D Int64
  in bench "UCyc.toPow" $ nf toPow x
