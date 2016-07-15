{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module TensorBenches (tensorBenches) where

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Types.ZqBasic
import Criterion
import Control.Monad.Random
import GHC.Magic

tensorBenches :: IO Benchmark
tensorBenches = do
  x :: CT F2048 (ZqBasic 12289 Int64) <- getRandom
  return $ bgroup "Tensor" [bench "l" $ nf l x,
                            bench "crt" $ nf (fromJust' "" crt) x]

{-
  benchGroup "Tensor" [
  benchGroup "l" $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_l]

-- convert input from Dec basis to Pow basis
bench_l :: (Tensor t, Fact m, Additive r, TElt t r, NFData (t m r)) => t m r -> Bench '(t,m,r)
bench_l = bench l

type QuickTest = '[ '(F128, Zq 257) ]
                    --'(F32 * F9, Zq 577),
                    --'(F32 * F9, Int64) ]
type Tensors = '[CT,RT]
type QuickParams = ( '(,) <$> Tensors) <*> QuickTest
-}