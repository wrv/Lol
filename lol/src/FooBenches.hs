
module FooBenches where

import Criterion
import Factored
import Foo
import T

benches :: Benchmark
benches =
  let x = scalarFoo 0 :: Foo F128
  in bench "foo" $ nf f x
