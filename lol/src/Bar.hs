{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Bar where

import Control.DeepSeq

import Factored
import Foo

newtype Bar t (m :: Factored) = Bar (t m) deriving (NFData)

{-# SPECIALIZE bar :: (Fact m) => Bar Foo m -> Bar Foo m #-}
{-# INLINABLE bar #-}
bar :: (Fact m, T t) => Bar t m -> Bar t m
bar (Bar v) = Bar $ t v
