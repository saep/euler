module Euler.P017
       ( solve
       ) where

solve :: Monad m => m Int
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
