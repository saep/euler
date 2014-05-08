module Main where

import qualified Euler.P001
import qualified Euler.P002
import qualified Euler.P003
import qualified Euler.P004
import qualified Euler.P005
import qualified Euler.P006
import qualified Euler.P007
import qualified Euler.P008
import qualified Euler.P009
import qualified Euler.P010
import qualified Euler.P011
import qualified Euler.P012
import qualified Euler.P013
import qualified Euler.P014
import qualified Euler.P015
import qualified Euler.P016
import qualified Euler.P017
import qualified Euler.P018
import qualified Euler.P019
import qualified Euler.P020
import qualified Euler.P021
import qualified Euler.P022
import qualified Euler.P023
import qualified Euler.P024
import qualified Euler.P025
import qualified Euler.P026
import qualified Euler.P027
import qualified Euler.P028
import qualified Euler.P029
import qualified Euler.P030
import qualified Euler.P031
import qualified Euler.P032
import qualified Euler.P033
import qualified Euler.P034
import qualified Euler.P035
import qualified Euler.P036
import qualified Euler.P037
import qualified Euler.P038
import qualified Euler.P039
import qualified Euler.P040
import qualified Euler.P041
import qualified Euler.P042
import qualified Euler.P043
import qualified Euler.P044
import qualified Euler.P045
import qualified Euler.P046
import qualified Euler.P047
import qualified Euler.P048
import qualified Euler.P049
import qualified Euler.P050
import qualified Euler.P051
import qualified Euler.P052
import qualified Euler.P053
import qualified Euler.P054
import qualified Euler.P055
import qualified Euler.P056

import Test.HUnit
import Test.Framework hiding (Test)
import Test.Framework.Providers.HUnit

main :: IO ()
main = Test.Framework.defaultMain
    [ testGroup "Hunit Tests" (hUnitTestToTests (test tests))
    ]

tests :: [Test]
tests =
  [ "Problem 1" ~: Euler.P001.solve >>= assertEqual "1" 233168
  , "Problem 2" ~: Euler.P002.solve >>= assertEqual "2" 4613732
  , "Problem 3" ~: Euler.P003.solve >>= assertEqual "3" 6857
  , "Problem 4" ~: Euler.P004.solve >>= assertEqual "4" 906609
  , "Problem 5" ~: Euler.P005.solve >>= assertEqual "5" 232792560
  , "Problem 6" ~: Euler.P006.solve >>= assertEqual "6" 25164150
  , "Problem 7" ~: Euler.P007.solve >>= assertEqual "7" 104743
  , "Problem 8" ~: Euler.P008.solve >>= assertEqual "8" 40824
  , "Problem 9" ~: Euler.P009.solve >>= assertEqual "9" 31875000
  , "Problem 10" ~: Euler.P010.solve >>= assertEqual "10" 142913828922
  , "Problem 11" ~: Euler.P011.solve >>= assertEqual "11" 70600674
  , "Problem 12" ~: Euler.P012.solve >>= assertEqual "12" 76576500
  , "Problem 13" ~: Euler.P013.solve >>= assertEqual "13" 5537376230
  , "Problem 14" ~: Euler.P014.solve >>= assertEqual "14" 837799
  , "Problem 15" ~: Euler.P015.solve >>= assertEqual "15" 137846528820
  , "Problem 16" ~: Euler.P016.solve >>= assertEqual "16" 1366
  , "Problem 17" ~: Euler.P017.solve >>= assertEqual "17" 21124
  , "Problem 18" ~: Euler.P018.solve >>= assertEqual "18" 1074
  , "Problem 19" ~: Euler.P019.solve >>= assertEqual "19" 171
  , "Problem 20" ~: Euler.P020.solve >>= assertEqual "20" 648
  , "Problem 21" ~: Euler.P021.solve >>= assertEqual "21" 31626
  , "Problem 22" ~: Euler.P022.solve >>= assertEqual "22" 871198282
  , "Problem 23" ~: Euler.P023.solve >>= assertEqual "23" 4179871
  , "Problem 24" ~: Euler.P024.solve >>= assertEqual "24" 2783915460
  , "Problem 25" ~: Euler.P025.solve >>= assertEqual "25" 4782
  , "Problem 26" ~: Euler.P026.solve >>= assertEqual "26" 983
  , "Problem 27" ~: Euler.P027.solve >>= assertEqual "27" (-59231)
  , "Problem 28" ~: Euler.P028.solve >>= assertEqual "28" 669171001
  , "Problem 29" ~: Euler.P029.solve >>= assertEqual "29" 9183
  , "Problem 30" ~: Euler.P030.solve >>= assertEqual "30" 443839
  , "Problem 31" ~: Euler.P031.solve >>= assertEqual "31" 73682
  , "Problem 32" ~: Euler.P032.solve >>= assertEqual "32" 45228
  , "Problem 33" ~: Euler.P033.solve >>= assertEqual "33" 100
  , "Problem 34" ~: Euler.P034.solve >>= assertEqual "34" 40730
  , "Problem 35" ~: Euler.P035.solve >>= assertEqual "35" 55
  , "Problem 36" ~: Euler.P036.solve >>= assertEqual "36" 872187
  , "Problem 37" ~: Euler.P037.solve >>= assertEqual "37" 748317
  , "Problem 38" ~: Euler.P038.solve >>= assertEqual "38" 932718654
  , "Problem 39" ~: Euler.P039.solve >>= assertEqual "39" 840
  , "Problem 40" ~: Euler.P040.solve >>= assertEqual "40" 210
  , "Problem 41" ~: Euler.P041.solve >>= assertEqual "41" 7652413
  , "Problem 42" ~: Euler.P042.solve >>= assertEqual "42" 162
  , "Problem 43" ~: Euler.P043.solve >>= assertEqual "43" 16695334890
  , "Problem 44" ~: Euler.P044.solve >>= assertEqual "44" 5482660
  , "Problem 45" ~: Euler.P045.solve >>= assertEqual "45" 1533776805
  , "Problem 46" ~: Euler.P046.solve >>= assertEqual "46" 5777
  , "Problem 47" ~: Euler.P047.solve >>= assertEqual "47" 134043
  , "Problem 48" ~: Euler.P048.solve >>= assertEqual "48" 9110846700
  , "Problem 49" ~: Euler.P049.solve >>= assertEqual "49" 296962999629
  , "Problem 50" ~: Euler.P050.solve >>= assertEqual "50" 997651
  , "Problem 51" ~: Euler.P051.solve >>= assertEqual "51" 121313
  , "Problem 52" ~: Euler.P052.solve >>= assertEqual "52" 142857
  , "Problem 53" ~: Euler.P053.solve >>= assertEqual "53" 4075
  , "Problem 54" ~: Euler.P054.solve >>= assertEqual "54" 376
  , "Problem 55" ~: Euler.P055.solve >>= assertEqual "55" 249
  , "Problem 56" ~: Euler.P056.solve >>= assertEqual "56" 972
  ]

