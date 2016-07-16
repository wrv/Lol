{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module TensorBenches (tensorBenches) where
{-
import Apply.Cyc
import Benchmarks
import Utils

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Lol.CRTrans

tensorBenches :: IO Benchmark
tensorBenches = benchGroup "Tensor" [
  --benchGroup "l" $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_l,
  benchGroup "crt" $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_crt]
-}

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Types.ZqBasic
import Criterion
import Control.Monad.Random
import GHC.Magic

tensorBenches :: IO Benchmark
tensorBenches = do
  x :: CT (F9*F5*F7*F11) (ZqBasic 34651 Int64) <- getRandom
  return $ bgroup "Tensor" [bench "crt" $ nf (fromJust' "" crt) x]
{-
-- convert input from Dec basis to Pow basis
bench_l :: (Tensor t, Fact m, Additive r, TElt t r, NFData (t m r)) => t m r -> Bench '(t,m,r)
bench_l = bench l

bench_crt :: (Tensor t, Fact m, TElt t r, NFData (t m r), CRTrans Maybe r) => t m r -> Bench '(t,m,r)
bench_crt = bench $ fromJust' "" crt

type QuickTest = '[ '(F9*F5*F7*F11, Zq 34651)
                  {-'(F128, Zq 257),
                    '(F32 * F9, Zq 577),
                    '(F32 * F9, Int64)-} ]
type Tensors = '[CT]
type QuickParams = ( '(,) <$> Tensors) <*> QuickTest
-}