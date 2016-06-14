{-# LANGUAGE TemplateHaskell #-}

module THApplyProxy where

import Data.Proxy
import Language.Haskell.TH

-- | Applies an expression @f@ to the 'Proxy' for a type @t@.
applyProxy :: ExpQ              -- ^ expression to apply
           -> Name              -- ^ name of the type to be proxied
           -> ExpQ
applyProxy f t = [| $f (Proxy::Proxy $(conT t)) |]
