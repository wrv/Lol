{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Wrapper for a C++ implementation of the 'Tensor' interface.

module Crypto.Lol.Cyclotomic.Tensor.CTensor
( CT ) where

import Control.Applicative    hiding ((*>))
import Control.DeepSeq
import Control.Monad.Except

import Data.Constraint              hiding ((***))
import Data.Int
import Data.Vector.Storable         as SV (Vector,
                                           generate,
                                           length,
                                           thaw, thaw,
                                           unsafeFreeze,
                                           unsafeWith)
import Data.Vector.Storable.Mutable as SM hiding (replicate)

import Foreign.Ptr

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
import Crypto.Lol.Prelude                             as LP hiding
                                                             (replicate,
                                                             unzip, zip)
import System.IO.Unsafe (unsafePerformIO)

-- | Newtype wrapper around a Vector.
newtype CT' (m :: Factored) r = CT' (Vector r)
                              deriving (Show, Eq, NFData)

-- the first argument, though phantom, affects representation
type role CT' representational nominal

-- GADT wrapper that distinguishes between Unbox and unrestricted
-- element types

-- | An implementation of 'Tensor' backed by C++ code.
data CT (m :: Factored) r where
  CT :: Storable r => CT' m r -> CT m r

deriving instance Show r => Show (CT m r)

wrap :: (Storable r) => (CT' l r -> CT' m r) -> (CT l r -> CT m r)
wrap f (CT v) = CT $ f v

instance Tensor CT where

  type TElt CT r = (Storable r, Dispatch r)

  entailNFDataT = tag $ Sub Dict

  scalarPow = CT . scalarPow' -- Vector code

  l = wrap $ untag $ basicDispatch dl

  {-# INLINABLE entailNFDataT #-}
  {-# INLINABLE scalarPow #-}
  {-# INLINABLE l #-}

withBasicArgs :: forall m r . (Fact m, Storable r)
  => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
     -> CT' m r -> IO (CT' m r)
withBasicArgs f =
  let factors = proxy (marshalFactors <$> ppsFact) (Proxy::Proxy m)
      totm = proxy (fromIntegral <$> totientFact) (Proxy::Proxy m)
      numFacts = fromIntegral $ SV.length factors
  in \(CT' x) -> do
    yout <- SV.thaw x
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        f pout totm pfac numFacts))
    CT' <$> unsafeFreeze yout

basicDispatch :: (Storable r, Fact m)
                 => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
                     -> Tagged m (CT' m r -> CT' m r)
basicDispatch f = return $ unsafePerformIO . withBasicArgs f

instance NFData (CT m r) where
  rnf (CT v) = rnf v

scalarPow' :: forall m r . (Fact m, Additive r, Storable r) => r -> CT' m r
-- constant-term coefficient is first entry wrt powerful basis
scalarPow' =
  let n = proxy totientFact (Proxy::Proxy m)
  in \r -> CT' $ generate n (\i -> if i == 0 then r else zero)
