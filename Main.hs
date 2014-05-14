{- |
Module      :  Main
Description :  Main module which allows access to all project euler riddles.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

This module gives convenient access to calculate the solution for any
exisiting and solved project euler riddle.  -}

module Main
       ( main
       ) where

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
import qualified Euler.P057
import qualified Euler.P058
import qualified Euler.P059

import qualified Data.IntMap         as M
import           Options.Applicative

data Options = Options { problem :: Int }

prettySolution :: Options -> IO ()
prettySolution (Options { problem = i}) = do
    putStr $ "Project Euler solution for riddle " ++ show i ++ ": "
    maybe (print "Solution not available.") (print =<<) $ M.lookup i problems

optionParser :: Parser Options
optionParser = Options <$> option
        (long "problem"
        <> short 'p'
        <> metavar "N"
        <> help "Problem number to test.")

main :: IO ()
main = execParser opts >>= prettySolution
  where
    opts = info (helper <*> optionParser)
      ( fullDesc
     <> progDesc "Print the solution for a project euler riddle."
     <> header "euler - A project euler solution producer ;-)" )

problems :: M.IntMap (IO Int)
problems = M.fromAscList $
       zip [1..] [ Euler.P001.solve
                 , Euler.P002.solve
                 , Euler.P003.solve
                 , Euler.P004.solve
                 , Euler.P005.solve
                 , Euler.P006.solve
                 , Euler.P007.solve
                 , Euler.P008.solve
                 , Euler.P009.solve
                 , Euler.P010.solve
                 , Euler.P011.solve
                 , Euler.P012.solve
                 , Euler.P013.solve
                 , Euler.P014.solve
                 , Euler.P015.solve
                 , Euler.P016.solve
                 , Euler.P017.solve
                 , Euler.P018.solve
                 , Euler.P019.solve
                 , Euler.P020.solve
                 , Euler.P021.solve
                 , Euler.P022.solve
                 , Euler.P023.solve
                 , Euler.P024.solve
                 , Euler.P025.solve
                 , Euler.P026.solve
                 , Euler.P027.solve
                 , Euler.P028.solve
                 , Euler.P029.solve
                 , Euler.P030.solve
                 , Euler.P031.solve
                 , Euler.P032.solve
                 , Euler.P033.solve
                 , Euler.P034.solve
                 , Euler.P035.solve
                 , Euler.P036.solve
                 , Euler.P037.solve
                 , Euler.P038.solve
                 , Euler.P039.solve
                 , Euler.P040.solve
                 , Euler.P041.solve
                 , Euler.P042.solve
                 , Euler.P043.solve
                 , Euler.P044.solve
                 , Euler.P045.solve
                 , Euler.P046.solve
                 , Euler.P047.solve
                 , Euler.P048.solve
                 , Euler.P049.solve
                 , Euler.P050.solve
                 , Euler.P051.solve
                 , Euler.P052.solve
                 , Euler.P053.solve
                 , Euler.P054.solve
                 , Euler.P055.solve
                 , Euler.P056.solve
                 , Euler.P057.solve
                 , Euler.P058.solve
                 , Euler.P059.solve
                 ]
