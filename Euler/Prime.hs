{-# LANGUAGE CPP #-}
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
       , isPrimeMillerRabin
#ifdef HTEST
       , powMod
#endif
       ) where

import           Control.Monad
import           Data.Bits
import           Data.List
import           Data.Ratio
import           Euler.SList

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.IntMap as M

import           Control.Monad.Primitive

fi :: (Integral n, Num n') => n -> n'
fi = fromIntegral

square :: Integral n => n -> n
square n = n*n

-- | Take the sqrt from the given integer value and round it down.
sqrt' :: Integral n => n -> n
sqrt' = floor . (sqrt :: Double -> Double) . fi

-- | Take the logarithm from the given integral for base b value and round it
-- down.
log' :: (Integral b, Integral n) => b -> n -> n
log' b = floor . logBase base . fi
    where
        base = fi b :: Double

-- | This brute-force prime test function is fairly efficient for small numbers
-- and if you do not need to query a lot of primes. Otherwise it is probably
-- better to use the miller rabin variant, especially for big numbers, or the
-- other 'isPrime''.
isPrime :: Int -> Bool
isPrime n = checkPrimality (takeWhile (\p -> p*p <= n) (2:[3,5..])) n

-- | Check for primality using the 'primes' stream from this module.  This will
-- be faster in the long run if you do a lot of prime checks.
isPrime' :: Int -> Bool
isPrime' n = checkPrimality (takeWhile (\p -> p*p <= n) primes) n

-- | Generic helper function to test the primality of a number by trial and
-- error division for all elements in the given list.
checkPrimality :: Integral n => [n] -> n -> Bool
checkPrimality ls n = n /= 1 && foldr test True ls
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
      | p*p >     n = reverse (n:ps)
      | otherwise   = f ps (p+1) n
      where
        (d,r) = n `divMod` p

-- | Prime factorization of the given integer using the 'primes' list of this
-- module.
factors :: Int -> [Int]
factors x = f primes x
  where
    f :: [Int] -> Int -> [Int]
    f (p:ps) n
      | p*p > n   = [ n | n > 1 ]
      | r == 0    = p : f (p:ps) d
      | otherwise = f ps n
      where
        (d,r) = n `divMod` p

eulerPhi :: Int -> Int
eulerPhi n =
    let ratio = (n%1 *) . product . fmap (\p -> 1%1 - 1%p) . snub $ factors n
    in numerator ratio `div` denominator ratio

-- | Calculate the sum of divisors for the given number.
sumOfDivisors :: Int -> Int
sumOfDivisors n =
    let fs = group $ factors n
    in product $ fmap ((+1) . sum . zipWith (flip (^)) ([1..] :: [Int])) fs

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

    mapM_ (flipEntries v)
        [ n | _4x2 <- takeWhile (<limit) $ map (flip shiftL 2 . square) [1..]
        , y2 <- takeWhile (<= limit - _4x2) $ map square [1..]
        , let n = _4x2 + y2
        , not (testBit n 1) && testBit n 0 && n `mod` 12 /= 9
        ]

    mapM_ (flipEntries v)
        [ n | _3x2 <- takeWhile (<limit) $ map ((3*) . square) [1..]
        , y2 <- takeWhile (<= limit - _3x2) $ map square [1..]
        , let n = _3x2 + y2
        , n `mod` 12 == 7
        ]

    mapM_ (flipEntries v)
        [ n | let l = ceiling $ sqrt (fromIntegral limit / 2)
        , x <- [2..l]
        , let _3x2 = 3*x*x
        , n <- takeWhile (<= limit) $ map (\y -> _3x2 - square y) [x-1,x-2..1]
        , n `mod` 12 == 11
        ]

    mapM_ (eliminateComposites v limit) [5..sqrtLimit]

    VM.unsafeWrite v 2 True
    VM.unsafeWrite v 3 True
    return v

flipEntries :: PrimMonad m => VM.MVector (PrimState m) Bool
              -> Int
              -> m ()
flipEntries v n = VM.unsafeWrite v n . not =<< VM.unsafeRead v n

eliminateComposites :: PrimMonad m => VM.MVector (PrimState m) Bool
                    -> Int
                    -> Int
                    -> m ()
eliminateComposites v limit n = do
    pn <- VM.unsafeRead v n
    when pn $ forM_ [n*n,2*n*n..limit] $ \i -> VM.unsafeWrite v i False

isPrimeMillerRabin :: (Bits n, Integral n) => n -> Bool
isPrimeMillerRabin n
    | n < 4000 = isPrime' (fromIntegral n)
    | otherwise =
        let ws = maybe (requiredValues n) snd $ M.lookupGT (fi n) witnesses
            fp = find2sd (n-1)
        in checkPrimality (takeWhile (< 200) primes) (fromIntegral n) &&
            (not . testMillerRabin n ws) fp

-- | Find @s@ and @d@, so that d*2^s = m
find2sd :: (Bits n, Integral n) => n -> (n,n)
find2sd = f 0
    where
        f s d
            | r == 1 = (s,d)
            | otherwise = f (s+1) q
                where
                    (q,r) = d `quotRem` 2

powMod :: (Bits n, Integral n) => n -> n -> n -> n
powMod m base ex = f 1 (base `mod` m) ex
    where
        f acc _ 0 = acc
        f acc b e = let acc' = if testBit e 0 then (acc*b) `mod` m else acc
                        b' = (b*b) `mod` m
                    in f acc' b' (shiftR e 1)

testMillerRabin :: (Bits n, Integral n) => n -> [n] -> (n, n) -> Bool
testMillerRabin n ws (s,d) = foldr test False ws
    where
        test a b = powMod n a d /= 1
                    && all (\r -> powMod n a (d * r) /= n-1) (fmap (shiftL 1) [fi s-1,fi s-2..0])
                    || b

requiredValues :: (Bits n, Integral n) => n -> [n]
requiredValues n =
    let lnn = (log . fi) n :: Double
        ub = min (n-1) $ floor (2 * lnn * lnn)
    in [2..ub]

witnesses :: Integral witness => M.IntMap [witness]
witnesses = M.fromList $
    [(1373653, [2,3])
    ,(9080191, [31,73])
    ,(4759123141, [2,7,61])
    ,(1122004669633, [2,13,23,1662803])
    ,(2152302898747, [2,3,5,7,11])
    ,(3474749660383,[2,3,5,7,11,13])
    ,(341550071728321,[2,3,5,7,11,13,17])
    ]

