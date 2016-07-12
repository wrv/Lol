
import TensorBenches
import UCycBenches

import Criterion.Main

main :: IO ()
main = defaultMain [
  tensorBenches,
  ucycBenches
  ]
