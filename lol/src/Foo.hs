{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Foo where

import Control.DeepSeq
import Control.Monad.ST

import Data.Int
import Data.Proxy
import Data.Tagged

import Factored

newtype Foo (m :: Factored) = Foo Int64 deriving (NFData)

class T t where
  t :: (Fact m ) => t m -> t m

instance T Foo where
  t = foo
  {-# INLINABLE t #-}

{-# INLINABLE foo #-}
foo :: forall m . (Fact m) => Foo m -> Foo m
foo = let totm = fromIntegral $ (proxy totientFact (Proxy::Proxy m))
      in \(Foo x) -> Foo $ x+totm

scalarFoo :: forall m . (Fact m) => Int64 -> Foo m
scalarFoo =
  let n = fromIntegral $ proxy totientFact (Proxy::Proxy m)
  in \r -> Foo $ r+n
