{-# LANGUAGE ConstraintKinds #-}

module Crypto.Lol.Types.Numeric
( module Crypto.Lol.Types.Numeric -- everything we define here
, module NumericPrelude         -- re-export
, Int64                         -- commonly used
) where

import           NumericPrelude         hiding (abs, max, min, (^))
import qualified Algebra.Additive             (C)

import Data.Int (Int64)

type Additive a = (Algebra.Additive.C a)
