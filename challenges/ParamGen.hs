{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

import Utils
import Crypto.Lol

-- number of ring-LWE samples
rlwel=[3,1000] :: [Int]

-- number of ring-LWR samples
rlwrl=[3,100000] :: [Int]

rlweq :: Factored -> Double -> [Int]
rlweq m v =
  let mval = valueF m
      mhat = fromIntegral $ valueHatF m
      n = fromIntegral $ totientF m
      r = sqrt v
      qmin = 8*sqrt((v+2*pi)*(2*mhat^2*(r+1)^2+1)*n) :: Double
      qmin' = ceiling qmin
  in [head $ goodQs mval qmin', head $ goodQs mval (qmin'*100)]

rlwev :: Factored -> Int -> [Double]
rlwev m l =
  let n' = fromIntegral $ totientF m :: Double
      l' = fromIntegral l
  in [16/n',                          -- narrow
      36/n',                          -- narrow
      64/n',                          -- narrow
      100/n',                         -- narrow
      4,                              -- conservative
      25*(sqrt $ n'*l'/(log $ n'*l')) -- worst-case hard
     ]

ms :: [Int]
ms = let maxPow2 = 7
         maxPow3 = 4
         maxPow5 = 3
         maxPow7 = 2
         exp2s = [0..maxPow2]
         exp3s = [0..maxPow3]
         exp5s = [0..maxPow5]
         exp7s = [0..maxPow7]
     in [2^i*3^j*5^k*7^l | i <- exp2s, j <- exp3s, k <- exp5s, l <- exp7s]