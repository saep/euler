{- |
Module      :  Euler.P059
Description :  Problem 59
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

XOR decryption

Each character on a computer is assigned a unique code and the preferred
standard is ASCII (American Standard Code for Information Interchange). For
example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII,
then XOR each byte with a given value, taken from a secret key. The advantage
with the XOR function is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text
message, and the key is made up of random bytes. The user would keep the
encrypted message and the encryption key in different locations, and without
both "halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method
is to use a password as a key. If the password is shorter than the message,
which is likely, the key is repeated cyclically throughout the message. The
balance for this method is using a sufficiently long password key for security,
but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case
characters. Using cipher1.txt (right click and 'Save Link/Target As...'), a file
containing the encrypted ASCII codes, and the knowledge that the plain text must
contain common English words, decrypt the message and find the sum of the ASCII
values in the original text.

-}
module Euler.P059
       ( solve
       ) where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as S

solve :: IO Int
solve = do
    ct <- readCipherText
    dict <- readDictionary
    let text = decrypt ct $ findPassword dict ct
    return . sum $ fmap ord text

readCipherText :: IO [Int]
readCipherText = read . ('[':) . (++"]") <$> readFile "./text/cipher1.txt"

readDictionary :: IO (S.Set String)
readDictionary = S.fromList . fmap (fmap toLower) . read . ('[':) . (++"]")
                    <$> readFile "./text/words.txt"

type CipherText = [Int]
type Password = [Int]

findPassword :: S.Set String -> CipherText -> Password
findPassword dict ct =
    let [d1,d2,d3] = fmap distribution $ split 3 ct
    in head $ [ [x1,x2,x3]
              | x1 <- xor <$> (take 3 lettersByProbablity) <*> (take 3 d1)
              , x2 <- xor <$> (take 3 lettersByProbablity) <*> (take 3 d2)
              , x3 <- xor <$> (take 3 lettersByProbablity) <*> (take 3 d3)
              , testDecryptedText dict ct [x1,x2,x3]
              ] ++ [findPasswordBruteForce dict ct]

findPasswordBruteForce :: S.Set String -> CipherText -> Password
findPasswordBruteForce dict ct =
    head [ [x,y,z] | x <- lettersByProbablity
         , y <- lettersByProbablity
         , z <- lettersByProbablity
         , testDecryptedText dict ct [x,y,z]
         ]

testDecryptedText :: S.Set String -> CipherText -> Password -> Bool
testDecryptedText dict c p = test (decrypt c p)
    where
        isInDictionary w = w `S.member` dict || init w `S.member` dict

        test :: String -> Bool
        test s = let xs = (words . fmap toLower) s
                 in f (length xs `div` 2) xs

        f 0 _ = False
        f _ [] = True
        f n (w:ws)
            | isInDictionary w = f n ws
            | otherwise        = f (n-1) ws

decrypt :: CipherText -> Password -> String
decrypt c p = zipWith toLowerCharacter c $ cycle p
    where
        toLowerCharacter x y = chr $ x `xor` y

split :: Int -> [a] -> [[a]]
split n = go (replicate n []) . splitAt n
    where
        go ret ([], _) = ret
        go ret (xs, []) = zipWith (:) xs ret ++ drop (length xs) ret
        go ret (xs, rs) = go (zipWith (:) xs ret) (splitAt n rs)


distribution :: (Bits a, Ord a) => [a] -> [a]
distribution s = fmap head . sortBy (flip compare `on` length) . group $ sort s

-- taken from http://en.wikipedia.org/wiki/Letter_frequency
lettersByProbablity :: [Int]
lettersByProbablity = fmap (ord . fst) $ sortBy (flip compare `on` snd)
    [(' ', 100.0) -- I'm actually only interested in the order of the letters
    ,('a',8.167)  -- and vim made it easy enough to create this list this way.
    ,('b',1.492)
    ,('c',2.782)
    ,('d',4.253)
    ,('e',12.702)
    ,('f',2.228)
    ,('g',2.015)
    ,('h',6.094)
    ,('i',6.966)
    ,('j',0.153)
    ,('k',0.772)
    ,('l',4.025)
    ,('m',2.406)
    ,('n',6.749)
    ,('o',7.507)
    ,('p',1.929)
    ,('q',0.095)
    ,('r',5.987)
    ,('s',6.327)
    ,('t',9.056)
    ,('u',2.758)
    ,('v',0.978)
    ,('w',2.360)
    ,('x',0.150)
    ,('y',1.974)
    ,('z',0.074)
    ]

