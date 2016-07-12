{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Crypto.Lol.Cyclotomic.Tensor.CTensor where

import Control.DeepSeq

import Data.Vector.Storable         as SV (Vector,
                                           generate,
                                           length,
                                           thaw, thaw,
                                           unsafeFreeze,
                                           unsafeWith)
import Data.Vector.Storable.Mutable as SM hiding (replicate)

import Crypto.Lol.FactoredDefs
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
import Data.Proxy
import Data.Tagged
import Data.Int
import System.IO.Unsafe (unsafePerformIO)

newtype CT (m :: Factored) = CT (Vector Int64) deriving (NFData)

l :: (Fact m) => CT m -> CT m
l = wrap ctl

-- EAC: why does this make Tensor as slow as UCyc?
--l' :: forall m . (Fact m) => CT m -> CT m
--l' (CT v) = CT $ untag (ctl :: Tagged m (Vector Int64 -> Vector Int64)) v

wrap :: Tagged m (Vector Int64 -> Vector Int64) -> (CT m -> CT m)
wrap f (CT v) = CT $ untag f v

ctl :: forall m . (Fact m)
  => Tagged m (Vector Int64 -> Vector Int64)
ctl =
  let factors = proxy (marshalFactors <$> ppsFact) (Proxy::Proxy m)
      totm = proxy (fromIntegral <$> totientFact) (Proxy::Proxy m)
      numFacts = fromIntegral $ SV.length factors
  in return $ \x -> unsafePerformIO $ do
    yout <- SV.thaw x
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        dl pout totm pfac numFacts))
    unsafeFreeze yout

scalarPow' :: Int64 -> Vector Int64
scalarPow' =
  let n = 64
  in \r -> generate n (\i -> if i == 0 then r else 0)
