
--import CycBenches
import TensorBenches
import UCycBenches
--import ZqBenches
import CTBenches
import Criterion.Main

main :: IO ()
main = defaultMain =<< sequence [
  --zqBenches,
  ctBenches,
  tensorBenches,
  ucycBenches
  --cycBenches
  ]
