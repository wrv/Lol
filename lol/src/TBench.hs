
module TBench where

import Criterion
import Factored
import Foo

benches :: Benchmark
benches =
  let x = scalarFoo 0 :: Foo F128
  in bench "T" $ nf t x
