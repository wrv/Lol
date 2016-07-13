
module FooBench where

import Criterion
import Factored
import Foo

benches :: Benchmark
benches =
  let x = scalarFoo 0 :: Foo F128
  in bench "Foo" $ nf foo x
