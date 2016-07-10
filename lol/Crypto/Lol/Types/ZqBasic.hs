{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, RoleAnnotations, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

module Crypto.Lol.Types.ZqBasic
( ZqBasic -- export the type, but not the constructor (for safety)
) where

import Crypto.Lol.Prelude           as LP
import Crypto.Lol.Reflects

import Control.Applicative
import Control.DeepSeq        (NFData)

-- for the Unbox instances
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

-- for the Elt instance
import qualified Data.Array.Repa.Eval as E

import qualified Algebra.Additive       as Additive (C)

-- | The ring \(\Z_q\) of integers modulo 'q', using underlying integer
-- type 'z'.
newtype ZqBasic q z = ZqB z
    deriving (Eq, Ord, E.Elt, Show, NFData, Storable)

-- the q argument, though phantom, matters for safety
type role ZqBasic nominal representational


{-# INLINABLE reduce' #-}
reduce' :: forall q z . (Reflects q z, ToInteger z) => z -> ZqBasic q z
reduce' = ZqB . (`mod` proxy value (Proxy::Proxy q))

-- instance of Additive
instance (Reflects q z, ToInteger z, Additive z) => Additive.C (ZqBasic q z) where

  {-# INLINABLE zero #-}
  zero = ZqB zero

  {-# INLINABLE (+) #-}
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (ZqB x) (ZqB y) ->
        let z = x + y
        in ZqB (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (ZqB x) = reduce' $ negate x

newtype instance U.MVector s (ZqBasic q z) = MV_ZqBasic (U.MVector s z)
newtype instance U.Vector (ZqBasic q z) = V_ZqBasic (U.Vector z)

-- Unbox, when underlying representation is
instance U.Unbox z => U.Unbox (ZqBasic q z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox z => M.MVector U.MVector (ZqBasic q z) where
  basicLength (MV_ZqBasic v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqBasic v) = MV_ZqBasic $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqBasic v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqBasic <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqB x) = MV_ZqBasic <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqBasic v) z = ZqB <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqBasic v) z (ZqB x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqBasic v) = M.basicClear v
  basicSet (MV_ZqBasic v) (ZqB x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqBasic v) n = MV_ZqBasic <$> M.basicUnsafeGrow v n

instance U.Unbox z => G.Vector U.Vector (ZqBasic q z) where
  basicUnsafeFreeze (MV_ZqBasic v) = V_ZqBasic <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqBasic v) = MV_ZqBasic <$> G.basicUnsafeThaw v
  basicLength (V_ZqBasic v) = G.basicLength v
  basicUnsafeSlice z n (V_ZqBasic v) = V_ZqBasic $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqBasic v) z = ZqB <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqBasic mv) (V_ZqBasic v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
