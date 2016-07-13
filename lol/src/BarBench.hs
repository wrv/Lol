
module BarBench where

import Bar
import Criterion
import Factored
import Foo

benches :: Benchmark
benches =
  let x = Bar $ scalarFoo 0 :: Bar Foo F128
  in bench "Bar" $ nf bar x
