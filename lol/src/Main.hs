
import FooBench
import BarBench
import TBench

import Criterion.Main

main :: IO ()
main = defaultMain [FooBench.benches, TBench.benches, BarBench.benches]
