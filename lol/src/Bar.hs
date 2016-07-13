{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Bar where

import Factored
import Foo
import T
import Control.DeepSeq

newtype Bar t (m :: Factored) = Bar (t m) deriving (NFData)

--{-# SPECIALIZE bar :: (Fact m) => Bar Foo m -> Bar Foo m #-}
bar :: (Fact m, T t) => Bar t m -> Bar t m
{-# INLINABLE bar #-}
bar (Bar v) = Bar $ f v