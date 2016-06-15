
import Utils

-- number of ring-LWE samples
rlwel=[3,1000]

-- number of ring-LWR samples
rlwrl=[3,100000]

rlweq :: forall m . (Fact m) => Proxy m -> Double -> [Int]
rlweq _ v =
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      n = proxy totientFact (Proxy::Proxy m)
      r = sqrt v
      qmin = 8*sqrt((v+2*pi)(2*mhat^2*(r+1)^2+1)*n)
  in [head $ goodQs m qmin, head $ goodQs m (qmin*100)]

rlwev :: forall m . (Fact m) => Proxy m -> Int -> [Double]
rlwev _ l =
  let n' = fromIntegral $ proxy totientFact (Proxy::Proxy m) :: Double
      l' = fromIntegral n
  in [16/n',                          -- narrow
      36/n',                          -- narrow
      64/n',                          -- narrow
      100/n',                         -- narrow
      4,                              -- conservative
      25*(sqrt $ n'*l'/(log $ n'*l')) -- worst-case hard
     ]

ms = let maxPow2 = 7
         maxPow3 = 4
         maxPow5 = 3
         maxPow7 = 2
         exp2s = [0..maxPow2]
         exp3s = [0..maxPow3]
         exp5s = [0..maxPow5]
         exp7s = [0..maxPow7]
     in [2^i*3^j*5^k*7^l | i <- exp2s, j <- exp3s, k <- exp5s, l <- exp7s]