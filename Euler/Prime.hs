module Euler.Prime
       ( sqrt'
       , factorizeSingleNumber
       , isPrime
       , isPrime'
       , factors
       , eulerPhi
       , divisors
       , sumOfProperDivisors
       , sumOfDivisors
       , coPrime
       , primes
       , sunion
       , PrimeSieve()
       , atkin
       , isPrimeS
       ) where

import Control.Monad
import Data.List
import Data.Ratio
import Euler.SList

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Control.Monad.Primitive

-- | Take the sqrt from the given integer value and round it down.
sqrt' :: Integral n => n -> n
sqrt' = floor . (sqrt :: Double -> Double) . fromIntegral

isPrime :: Int -> Bool
isPrime n = n /= 1 && foldr test True [2..sqrt' n]
  where
    test d r = n `mod` d /= 0 && r

-- | Check for primality using the 'primes' stream from this module.  This will
-- be faster in the long run if you do a lot of prime checks.
isPrime' :: Int -> Bool
isPrime' n = n /= 1 && foldr test True (takeWhile (<= sqrt' n) primes)
    where
        test d r = n `mod` d /= 0 && r

-- | A black box type obfuscation to be able to replace it later with something
-- else.
type PrimeSieve = V.Vector Bool

isPrimeS :: PrimeSieve -> Int -> Bool
isPrimeS = (V.!)

-- | Factorize a single number by testing all nmbers up to the square
-- root of the given number.
factorizeSingleNumber :: Int -> [Int]
factorizeSingleNumber = f [] 2
  where
    f ps p n
      | r == 0      = f (p:ps) p d
      | p > sqrt' n = reverse (n:ps)
      | otherwise   = f ps (p+1) n
      where
        (d,r) = n `divMod` p

-- | Prime factorization of the given integer using the 'primes' list of this
-- module.
factors :: Int -> [Int]
factors x = f (sqrt' x) primes x
  where
    f :: Int-> [Int] -> Int -> [Int]
    f sqrtn (p:ps) n
      | p > sqrtn  = [ n | n > 1 ]
      | r == 0 = p : f (sqrt' d) (p:ps) d
      | otherwise = f sqrtn ps n
      where
        (d,r) = n `divMod` p

eulerPhi :: Int -> Int
eulerPhi n =
    let ratio = (n%1 *) . product . fmap (\p -> 1%1 - 1%p) . snub $ factors n
    in numerator ratio `div` denominator ratio

-- | Calculate the sum of divisors for the given number.
sumOfDivisors :: Int -> Int
sumOfDivisors n =
    let fs = factors n
    in product . fmap ((+1) . sum . zipWith (flip (^)) ([1..] :: [Int])) $ group fs

-- | Calculate the sum of proper divisors of the given number.
sumOfProperDivisors :: Int -> Int
sumOfProperDivisors n = subtract n $ sumOfDivisors n

divisors :: Int -> Int
divisors = product . fmap (succ . length) . group . factors

-- | Two integers are coprime if their common denominator is 1.
coPrime :: Integral n => n -> n -> Bool
coPrime a b = 1 == gcd a b

primes :: [Int]
primes = primesTMWE

_Y :: (t -> t) -> t
_Y g = g (_Y g)

joinT :: Ord a => [[a]] -> [a]
joinT ((x:xs):t) = x : (sunion xs . joinT . pairs) t
  where
    pairs :: Ord a => [[a]] -> [[a]]
    pairs (xxs:yys:tt) = sunion xxs yys : pairs tt

primesTMWE :: [Int]
primesTMWE = [2,3,5,7] ++ _Y ((11:) . tail . gapsW 11 wheel
                              . joinT . hitsW 11 wheel)

gapsW :: Int -> [Int] -> [Int] -> [Int]
gapsW k (d:w) s@(c:cs)
  | k < c = k : gapsW (k+d) w s
  | otherwise = gapsW (k+d) w cs

hitsW :: Int -> [Int] -> [Int] -> [[Int]]
hitsW k (d:w) s@(p:ps)
  | k < p = hitsW (k+d) w s
  | otherwise = scanl (\c d' -> c+p*d') (p*p) (d:w) : hitsW (k+d) w ps

wheel :: [Int]
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

atkin :: Int -> V.Vector Bool
atkin limit = V.create $ do
    v <- VM.replicate (limit+1) False

    let sqrtLimit = sqrt' limit

    mapM_ (flipEntries v limit) [ (x,y)
                                | x <- [1..sqrtLimit]
                                , y <- [1..sqrtLimit]
                                ]

    mapM_ (eliminateComposites v limit) [5..sqrtLimit]

    VM.unsafeWrite v 2 True
    VM.unsafeWrite v 3 True
    return v

flipEntries :: PrimMonad m => VM.MVector (PrimState m) Bool
            -> Int
            -> (Int, Int)
            -> m ()
flipEntries v limit (x,y) = do
    let n = 4*x*x + y*y
    when (n <= limit && (n `mod` 12) `elem` [1,5])
        $ VM.unsafeWrite v n . not =<< VM.unsafeRead v n
    let o = n - x*x
    when (o <= limit && (o `mod` 12) == 7)
        $ VM.unsafeWrite v o . not =<< VM.unsafeRead v o
    let p = o - 2*y*y
    when (x > y && p <= limit && p `mod` 12 == 11)
        $ VM.unsafeWrite v p . not =<< VM.unsafeRead v p

eliminateComposites :: PrimMonad m => VM.MVector (PrimState m) Bool
                    -> Int
                    -> Int
                    -> m ()
eliminateComposites v limit n = do
    pn <- VM.unsafeRead v n
    when pn $ forM_ [n*n,2*n*n..limit] $ \i -> VM.unsafeWrite v i False

