
module FooBenches where

import Factored
import FooBar

import Criterion

benches :: Benchmark
benches =
  let x = scalarFoo 0 :: Foo F128
  in bench "foo" $ nf foo x
