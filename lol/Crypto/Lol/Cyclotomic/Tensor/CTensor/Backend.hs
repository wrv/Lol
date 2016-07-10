{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif

-- | This module contains the functions to transform Haskell types into their
-- C counterpart, and to transform polymorphic Haskell functions into C funtion
-- calls in a type-safe way.

module Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend where

import Control.Applicative

import Crypto.Lol.Prelude       as LP (PP, Proxy (..),
                                       map, proxy, (++))
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList)

import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr)
import           Foreign.Storable        (Storable (..))
import qualified Foreign.Storable.Record as Store

-- | Convert a list of prime powers to a suitable C representation.
marshalFactors :: [PP] -> Vector CPP
marshalFactors = SV.fromList . LP.map (\(p,e) -> CPP (fromIntegral p) (fromIntegral e))

-- | C representation of a prime power.
data CPP = CPP {p' :: !Int32, e' :: !Int16}
-- stolen from http://hackage.haskell.org/packages/archive/numeric-prelude/0.4.0.3/doc/html/src/Number-Complex.html#T
-- the NumericPrelude Storable instance for complex numbers
instance Storable CPP where
   sizeOf    = Store.sizeOf store
   alignment = Store.alignment store
   peek      = Store.peek store
   poke      = Store.poke store

store :: Store.Dictionary CPP
store = Store.run $
   liftA2 CPP
      (Store.element p')
      (Store.element e')

instance Show CPP where
    show (CPP p e) = "(" LP.++ show p LP.++ "," LP.++ show e LP.++ ")"

-- | Class to safely match Haskell types with the appropriate C function.
class Dispatch r where

  dl        :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()

instance (Reflects q Int64) => Dispatch (ZqBasic q Int64) where
  dl pout totm pfac numFacts =
    let qs = proxy value (Proxy::Proxy q) :: Int64
    in with qs $ \qsptr ->
        tensorLRq 1 (castPtr pout) totm pfac numFacts (castPtr qsptr)

foreign import ccall unsafe "tensorLRq" tensorLRq ::                Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
