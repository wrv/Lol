{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

module Crypto.Lol.Cyclotomic.UCyc
(
  UCyc, P, D, toPow
) where

import Crypto.Lol.Cyclotomic.Tensor hiding (scalarPow)

import Crypto.Lol.Factored
import qualified Crypto.Lol.Cyclotomic.Tensor      as T
import           Crypto.Lol.Types.Numeric                as LP

import qualified Algebra.Additive     as Additive (C)

import Control.DeepSeq
import Data.Tagged

-- | Nullary index type representing the powerful basis.
data P
-- | Nullary index type representing the decoding basis.
data D

data UCyc t (m :: Factored) rep r where
  Pow  :: !(t m r) -> UCyc t m P r
  Dec  :: !(t m r) -> UCyc t m D r

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m D r) where
  zero = Dec $ T.scalarPow zero
  {-# INLINABLE zero #-}

toPow :: (Fact m, Tensor t, TElt t r, Additive r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v

instance (Tensor t, Fact m, NFData r, TElt t r)
         => NFData (UCyc t m rep r) where
  rnf (Pow x)      = rnf x \\ witness entailNFDataT x
  rnf (Dec x)      = rnf x \\ witness entailNFDataT x
