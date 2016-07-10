{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Crypto.Lol.Cyclotomic.Tensor where

import Crypto.Lol.Prelude           as LP hiding (lift, (*>))
import           Control.DeepSeq
import           Data.Constraint

class Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  entailNFDataT :: Tagged (t m r)
                   ((NFData r, Fact m, TElt t r) :- NFData (t m r))

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Additive r, Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l :: (Additive r, Fact m, TElt t r) => t m r -> t m r
