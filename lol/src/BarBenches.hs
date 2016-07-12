
module BarBenches where

import Factored
import FooBar
import Criterion

benches :: Benchmark
benches =
  let x = Bar $ scalarFoo 0 :: Bar F128
  in bench "bar" $ nf bar x
