{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}

module Crypto.Lol.Cyclotomic.UCyc where

import Crypto.Lol.Factored
import Crypto.Lol.Cyclotomic.Tensor.CTensor

import Control.DeepSeq

import Data.Int

data UCyc (m :: Factored) = Dec !(CT m Int64)

toPow :: (Fact m) => UCyc m -> UCyc m
{-# INLINABLE toPow #-}
toPow (Dec v) = Dec $ wrap ctl v

instance NFData (UCyc m) where
  rnf (Dec x) = rnf x
