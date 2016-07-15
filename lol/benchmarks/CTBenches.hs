
{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module CTBenches (ctBenches) where

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Criterion
import Crypto.Lol.Types.ZqBasic
--import Utils
import Control.Monad.Random

ctBenches :: IO Benchmark
ctBenches = do
  x :: CT F2048 (ZqBasic 12289 Int64) <- getRandom
  return $ bgroup "CT" [bench "l" $ nf (wrap l') x,
                        bench "crt" $ nf (wrap $ fromJust' "" crt') x]
