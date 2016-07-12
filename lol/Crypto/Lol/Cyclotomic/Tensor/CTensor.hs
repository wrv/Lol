{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Crypto.Lol.Cyclotomic.Tensor.CTensor where

import Control.DeepSeq

import Data.Vector.Storable         as SV (Vector,
                                           generate,
                                           length,
                                           thaw, thaw,
                                           unsafeFreeze,
                                           unsafeWith)
import Data.Vector.Storable.Mutable as SM hiding (replicate)

import Crypto.Lol.Factored
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
import Data.Proxy
import Data.Tagged

import System.IO.Unsafe (unsafePerformIO)

-- | An implementation of 'Tensor' backed by C++ code.
data CT (m :: Factored) r where
  CT :: Storable r => Vector r -> CT m r

deriving instance Show r => Show (CT m r)

wrap :: (Storable r) => Tagged m (Vector r -> Vector r) -> (CT m r -> CT m r)
wrap f (CT v) = CT $ untag f v

ctl :: forall m r . (Fact m, Storable r, Dispatch r)
  => Tagged m (Vector r -> Vector r)
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

instance NFData (CT m r) where
  rnf (CT v) = rnf v

scalarPow' :: forall r . (Num r, Storable r) => r -> Vector r
-- constant-term coefficient is first entry wrt powerful basis
scalarPow' =
  let n = 64 -- proxy totientFact (Proxy::Proxy m)
  in \r -> generate n (\i -> if i == 0 then r else 0)
