
module BarBenches where

import Criterion
import Factored
import FooBar

benches :: Benchmark
benches =
  let x = Bar $ scalarFoo 0 :: Bar F128
  in bench "bar" $ nf bar x
