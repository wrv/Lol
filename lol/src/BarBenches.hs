
module BarBenches where

import Criterion
import Factored
import Bar
import Foo

benches :: Benchmark
benches =
  let x = Bar $ scalarFoo 0 :: Bar Foo F128
  in bench "bar" $ nf bar x
