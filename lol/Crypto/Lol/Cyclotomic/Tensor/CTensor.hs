{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Crypto.Lol.Cyclotomic.Tensor.CTensor where

import Control.DeepSeq

import Data.Vector.Storable         as SV (Vector,
                                           generate,
                                           thaw, thaw,
                                           unsafeFreeze)
import Data.Vector.Storable.Mutable as SM hiding (replicate)

import Crypto.Lol.FactoredDefs
import Control.Monad.ST
import Data.Proxy
import Data.Tagged
import Data.Int


newtype UCyc (m :: Factored) = Dec (CT m) deriving (NFData)

toPow :: (Fact m) => UCyc m -> UCyc m
{-# INLINABLE toPow #-}
toPow (Dec v) = Dec $ l v

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
  let [(p,e)] = proxy (ppsFact) (Proxy::Proxy m)
      totm = fromIntegral $ p*e*(proxy totientFact (Proxy::Proxy m))
  in return $ \x -> runST $ do
       yout <- SV.thaw x
       SM.modify yout (+totm) 0
       unsafeFreeze yout

scalarPow' :: Int64 -> Vector Int64
scalarPow' =
  let n = 64
  in \r -> generate n (\i -> if i == 0 then r else 0)

