{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Crypto.Lol.Cyclotomic.UCyc where

import Crypto.Lol.FactoredDefs
import Crypto.Lol.Cyclotomic.Tensor.CTensor

import Control.DeepSeq

newtype UCyc (m :: Factored) = Dec (CT m) deriving (NFData)

toPow :: (Fact m) => UCyc m -> UCyc m
{-# INLINABLE toPow #-}
toPow (Dec v) = Dec $ l v