{- |
Module      :  Main
Description :  Main module which allows access to all project euler riddles.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

This module gives convenient access to calculate the solution for any
exisiting and solved project euler riddle.
-}

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

prettySolution :: (Int, IO ()) -> IO ()
prettySolution (i, solution) =
  do putStr $ "Project Euler solution for riddle " ++ show i ++ ": "
     solution

main :: IO ()
main = mapM_ prettySolution $
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
                 ]
