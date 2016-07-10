{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Cyclotomic.UCyc
(
-- * Data types and constraints
  UCyc, P, D, toPow
) where

import Crypto.Lol.Cyclotomic.Tensor hiding (scalarPow)

import qualified Crypto.Lol.Cyclotomic.Tensor      as T
import           Crypto.Lol.Prelude                as LP

import qualified Algebra.Additive     as Additive (C)

import Control.DeepSeq

-- | Nullary index type representing the powerful basis.
data P
-- | Nullary index type representing the decoding basis.
data D

data UCyc t (m :: Factored) rep r where
  Pow  :: !(t m r) -> UCyc t m P r
  Dec  :: !(t m r) -> UCyc t m D r

type UCRTElt t r = (Tensor t, TElt t r)

type NFElt r = (NFData r)

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m D r) where
  zero = Dec $ T.scalarPow zero
  {-# INLINABLE zero #-}

toPow :: (Fact m, UCRTElt t r, Additive r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v

instance (Tensor t, Fact m, NFElt r, TElt t r)
         => NFData (UCyc t m rep r) where
  rnf (Pow x)      = rnf x \\ witness entailNFDataT x
  rnf (Dec x)      = rnf x \\ witness entailNFDataT x
