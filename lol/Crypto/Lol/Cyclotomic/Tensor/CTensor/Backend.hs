
module Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend where

import Control.Applicative

import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList)

import           Foreign.Ptr             (Ptr, castPtr)
import           Foreign.Storable        (Storable (..))
import qualified Foreign.Storable.Record as Store

-- | Convert a list of prime powers to a suitable C representation.
marshalFactors :: [(Int,Int)] -> Vector CPP
marshalFactors = SV.fromList . map (\(p,e) -> CPP (fromIntegral p) (fromIntegral e))

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
    show (CPP p e) = "(" ++ show p ++ "," ++ show e ++ ")"

dl :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dl pout = tensorLR 1 (castPtr pout)

foreign import ccall unsafe "tensorLR" tensorLR :: Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()