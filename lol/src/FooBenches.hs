
module FooBenches where

import Criterion
import Factored
import FooBar

benches :: Benchmark
benches =
  let x = scalarFoo 0 :: Foo F128
  in bench "foo" $ nf foo x
