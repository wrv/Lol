{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, GADTs,
             MultiParamTypeClasses, NoImplicitPrelude, RankNTypes,
             RebindableSyntax, ScopedTypeVariables #-}

-- | The @G@ and @L@ transforms for Repa arrays.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
( fL
) where

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon as RT
import Crypto.Lol.Prelude                               as LP

fL :: (Fact m, Additive r, Unbox r, Elt r)
  => Arr m r -> Arr m r
{-# INLINABLE fL #-}

-- | Arbitrary-index @L@ transform, which converts from decoding-basis
-- to powerful-basis representation.
fL = eval $ fTensor $ ppTensor pL

pWrap :: forall p r . Prime p
         => (forall rep . Source rep r => Int -> Array rep DIM2 r -> Array D DIM2 r)
         -> Tagged p (Trans r)
pWrap f = let pval = proxy valuePrime (Proxy::Proxy p)
              -- special case: return identity function for p=2
          in return $ if pval > 2
                      then trans  (pval-1) $ f pval
                      else Id 1
{-# INLINABLE pWrap #-}


pL :: (Prime p, Additive r, Elt r, Unbox r) => Tagged p (Trans r)
{-# INLINABLE pL #-}

pL = pWrap (\_ !arr ->
             fromFunction (extent arr) $
             \ (i':.i) -> sumAllS $ extract (Z:.0) (Z:.(i+1)) $ slice arr (i':.All))
