{- |
Module      :  Euler.P017
Description :  Problem 017
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Number letter counts

If the numbers 1 to 5 are written out in words: one, two, three, four,
five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were
written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
and forty-two) contains 23 letters and 115 (one hundred and fifteen)
contains 20 letters. The use of "and" when writing out numbers is in
compliance with British usage.

-}
module Euler.P017
       ( solve
       ) where

solve :: IO Int
solve = return $ length "one" * ((1 + 8) * 10 + 1 + 100)
        --
        + ((1+8) * 10 + 100) * sum [ length "two"
                                   , length "three"
                                   , length "four"
                                   , length "five"
                                   , length "six"
                                   , length "seven"
                                   , length "eight"
                                   , length "nine"
                                   ]
        --
        + (10*10) * sum [ length "twenty"
                        , length "thirty"
                        , length "forty"
                        , length "fifty"
                        , length "sixty"
                        , length "seventy"
                        , length "eighty"
                        , length "ninety"
                        ]
        --
        + 891 * length "and"
        --
        + 900 * length "hundred"
        + length "thousand"
        --
        + 10 * sum [ length "ten"
                   , length "eleven"
                   , length "twelve"
                   , length "thirteen"
                   , length "fourteen"
                   , length "fifteen"
                   , length "sixteen"
                   , length "seventeen"
                   , length "eighteen"
                   , length "nineteen"
                   ]
