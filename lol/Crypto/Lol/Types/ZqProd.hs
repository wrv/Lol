{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Crypto.Lol.Types.ZqProd (ZqProd, toZqBasic, fromZqBasic) where

import Algebra.ZeroTestable as ZeroTestable
import Algebra.Additive as Additive
import Algebra.IntegralDomain as ID
import Algebra.Ring as Ring

import Control.Applicative
import Control.Monad.Except

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.ZqBasic
import qualified Crypto.Proto.RLWE.ZqProd as P

import qualified Data.Sequence as S
import Data.Vector.Storable (Storable)

-- for the Unbox instances
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import System.Random

type family ZqTup (qs :: [k]) i where
  ZqTup '[q] i = ZqBasic q i
  ZqTup (q ': qs) i = (ZqBasic q i, ZqTup qs i)

-- EAC: roles are all nominal due to use of type family
-- https://ghc.haskell.org/trac/ghc/ticket/8177
newtype ZqProd qs i = ZqProd (ZqTup qs i)
  deriving (Additive.C, ID.C, Ring.C, Storable, Random, Reduce a, ZeroTestable.C)

-- EAC: can't derive for a generic monad due to unknown roles of `m`
instance (CRTrans m (ZqTup qs i), Monad m, Ring (ZqTup qs i)) => CRTrans m (ZqProd qs i) where
  crtInfo = do
    (f, minv) <- crtInfo
    return (ZqProd . f, ZqProd minv)

-- EAC: can't derive due to associated type
instance (Ring (ZqProd qs z), CRTEmbed (ZqTup qs z)) => CRTEmbed (ZqProd qs z) where
  -- should we make a "Complex product" instead?
  type CRTExt (ZqProd qs z) = CRTExt (ZqTup qs z)

  toExt (ZqProd x) = toExt x
  fromExt x = ZqProd $ fromExt x

-- EAC: must standalone-derive
-- https://ghc.haskell.org/trac/ghc/ticket/11008
deriving instance (Lift' (ZqTup qs z)) => Lift' (ZqProd qs z)
type instance LiftOf (ZqProd qs z) = LiftOf (ZqTup qs z)

{-
-- may not need this
-- ModRep can't be [z] to to superclass constraints on Mod
instance (Reflects qs [z], ToInteger z, Additive (ZqProd qs z)) => Mod (ZqProd qs z) where
  type ModRep (ZqProd qs z) = Integer

  modulus = tag $ product $ map fromIntegral $ (proxy value (Proxy::Proxy qs) :: [z])
-}

-- | Embed a single 'ZqBasic' into a 'ZqProd'.
fromZqBasic :: ZqBasic q i -> ZqProd '[q] i
fromZqBasic = ZqProd

-- | Extract a single 'ZqBasic' from a 'ZqProd'.
toZqBasic :: ZqProd '[q] i -> ZqBasic q i
toZqBasic  (ZqProd x) = x

instance (Protoable (ZqBasic q Int64)) => Protoable (ZqProd '[q] Int64) where
  type ProtoType (ZqProd '[q] Int64) = P.ZqProd

  toProto (ZqProd zqb) = toProto zqb
  fromProto s = ZqProd <$> fromProto s

instance (Protoable (ZqBasic q1 Int64),
          Protoable (ZqProd (q2 ': qs) Int64),
          Reflects (q1 ': q2 ': qs) [Int64],
          ProtoType (ZqProd (q2 ': qs) Int64) ~ P.ZqProd)
  => Protoable (ZqProd (q1 ': q2 ': qs) Int64) where
  type ProtoType (ZqProd (q1 ': q2 ': qs) Int64) = P.ZqProd

  toProto (ZqProd (a,bs)) =
    let (P.ZqProd a') = toProto a
        (P.ZqProd bs') = toProto $ (ZqProd bs :: ZqProd (q2 ': qs) Int64)
    in P.ZqProd $ a' S.>< bs'

  fromProto (P.ZqProd s) = do
    let qs = proxy value (Proxy::Proxy (q1 ': q2 ': qs)) :: [Int64]
    unless (S.length s == length qs) $ throwError $
      "Expected a tuple of size " ++ (show $ length qs) ++
      ", but received a tuple of size " ++ (show $ S.length s)
    a <- fromProto $ P.ZqProd $ S.singleton $ S.index s 0
    (ZqProd bs) :: ZqProd (q2 ': qs) Int64 <- fromProto $ P.ZqProd $ S.drop 1 s
    return $ ZqProd (a,bs)

newtype instance U.MVector s (ZqProd qs z) = MV_ZqProd (U.MVector s (ZqTup qs z))
newtype instance U.Vector (ZqProd qs z) = V_ZqProd (U.Vector (ZqTup qs z))

-- Unbox, when underlying representation is
instance U.Unbox (ZqTup qs z) => U.Unbox (ZqProd qs z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox (ZqTup qs z) => M.MVector U.MVector (ZqProd qs z) where
  basicLength (MV_ZqProd v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqProd v) = MV_ZqProd $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqProd v1) (MV_ZqProd v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqProd v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqProd <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqProd x) = MV_ZqProd <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqProd v) z = ZqProd <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqProd v) z (ZqProd x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqProd v) = M.basicClear v
  basicSet (MV_ZqProd v) (ZqProd x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqProd v1) (MV_ZqProd v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqProd v1) (MV_ZqProd v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqProd v) n = MV_ZqProd <$> M.basicUnsafeGrow v n

instance U.Unbox (ZqTup qs z) => G.Vector U.Vector (ZqProd qs z) where
  basicUnsafeFreeze (MV_ZqProd v) = V_ZqProd <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqProd v) = MV_ZqProd <$> G.basicUnsafeThaw v
  basicLength (V_ZqProd v) = G.basicLength v
  basicUnsafeSlice z n (V_ZqProd v) = V_ZqProd $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqProd v) z = ZqProd <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqProd mv) (V_ZqProd v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
