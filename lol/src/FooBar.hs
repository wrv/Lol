{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module FooBar where

import Control.DeepSeq
import Control.Monad.ST

import Data.Int
import Data.Proxy
import Data.Tagged
import Data.Vector.Storable as SV (Vector, generate, thaw, unsafeFreeze)
import Data.Vector.Storable.Mutable as SM

import Factored

newtype Bar (m :: Factored) = Bar (Foo m) deriving (NFData)

bar :: (Fact m) => Bar m -> Bar m
{-# INLINABLE bar #-}
bar (Bar v) = Bar $ foo v

newtype Foo (m :: Factored) = Foo (Vector Int64) deriving (NFData)

foo :: forall m . (Fact m) => Foo m -> Foo m
foo = let [(p,e)] = proxy ppsFact (Proxy::Proxy m)
          totm = fromIntegral $ p*e*(proxy totientFact (Proxy::Proxy m))
      in \(Foo x) -> Foo $ runST $ do
           yout <- SV.thaw x
           SM.modify yout (+totm) 0
           unsafeFreeze yout

scalarFoo :: forall m . (Fact m) => Int64 -> Foo m
scalarFoo =
  let n = proxy totientFact (Proxy::Proxy m)
  in \r -> Foo $ generate n (\i -> if i == 0 then r else 0)
