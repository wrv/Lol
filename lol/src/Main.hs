
import FooBenches
import BarBenches

import Criterion.Main

main :: IO ()
main = defaultMain [FooBenches.benches, BarBenches.benches]
