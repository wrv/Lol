{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.Lol.Cyclotomic.Tensor where

import Crypto.Lol.Factored
import Crypto.Lol.Types.Numeric
import Control.DeepSeq
import Data.Constraint
import Data.Tagged

class Tensor t where

  type TElt t r :: Constraint

  entailNFDataT :: Tagged (t m r)
                   ((NFData r, Fact m, TElt t r) :- NFData (t m r))

  scalarPow :: (Additive r, Fact m, TElt t r) => r -> t m r

  l :: (Additive r, Fact m, TElt t r) => t m r -> t m r
