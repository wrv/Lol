{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A pure, repa-based implementation of the 'Tensor' interface.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor
( RT ) where

import Crypto.Lol.Cyclotomic.Tensor                      as T
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon  as RT hiding
                                                                ((++))
import Crypto.Lol.Prelude                                as LP

import Algebra.Additive     as Additive (C)

import Control.DeepSeq      (NFData (rnf))
import Data.Constraint      hiding ((***))

-- | An implementation of 'Tensor' backed by repa.
data RT (m :: Factored) r where
  RT :: Unbox r => !(Arr m r) -> RT m r

deriving instance Show r => Show (RT m r)

{-# INLINABLE wrap #-}
wrap :: Unbox r => (Arr l r -> Arr m r) -> RT l r -> RT m r
wrap f (RT v) = RT $ f v

instance Tensor RT where

  type TElt RT r = (Unbox r, Elt r)

  entailNFDataT = tag $ Sub Dict

  scalarPow = RT . scalarPow'

  l = wrap fL

  {-# INLINABLE entailNFDataT #-}
  {-# INLINABLE scalarPow #-}
  {-# INLINABLE l #-}

instance (Unbox r, Additive (Arr m r)) => Additive.C (RT m r) where
  zero = RT zero

instance NFData (RT m r) where
  rnf (RT v) = rnf v
