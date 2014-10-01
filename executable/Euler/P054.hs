module Euler.P054
       ( solve
       ) where

import           Control.Arrow ((***))
import           Data.Function (on)
import           Data.List     (group, sort, sortBy)
import qualified Data.Map      as M
import           Data.Maybe

data CardType = Two | Three | Four | Five | Six | Seven | Eight
              | Nine | Ten | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord, Show, Read)

data Color = Diamonds | Hearts | Clubs | Spades
    deriving (Enum, Eq, Ord, Show, Read)

type Card = (CardType, Color)

newtype Hand = Hand { getHand :: [Card] }
    deriving (Show)

data HandType = HighCard [CardType]
              | OnePair CardType [CardType]
              -- | high pair type -- low pair type -- high card
              | TwoPairs CardType CardType CardType
              | ThreeOfaKind CardType
              | Straight CardType
              | Flush [CardType]
              | FullHouse CardType
              | FourOfaKind CardType
              | StraightFlush CardType
              | RoyalFlush
    deriving (Eq, Ord, Show)

parseHands :: [String] -> (Hand, Hand)
parseHands = (Hand *** Hand) . splitAt 5 . fmap parseCard

stringToCardMap :: M.Map Char CardType
stringToCardMap = M.fromList $ zip "23456789TJQKA" (fmap toEnum [0..maxBound])

parseCard :: String -> Card
parseCard (x:y:_) = (fromJust (M.lookup x stringToCardMap), parseColor y)

parseColor :: Char -> Color
parseColor y = case y of
    'D' -> Diamonds
    'H' -> Hearts
    'C' -> Clubs
    'S' -> Spades
    _ -> error "undefined card type"

solve :: IO Int
solve = do
    f <- readFile "text/poker.txt"
    let rawHands = fmap (parseHands . words) $ lines f
        hands = fmap (defineHandType *** defineHandType) rawHands
        p1wins = length $ filter (uncurry (>)) hands
    return p1wins

defineHandType :: Hand -> HandType
defineHandType h = case (sortByLength . group . sort . fmap fst . getHand) h of
    [_,[c,_,_,_]] -> FourOfaKind c
    [[_,_],[c,_,_]] -> FullHouse c
    [_,_,[c,_,_]]     -> ThreeOfaKind c
    [[hc],[c,_],[x,_]] -> TwoPairs x c hc
    [[c1],[c2],[c3],[p,_]] -> OnePair p [c3,c2,c1]
    _ | isRoyalFlush h -> RoyalFlush
    cs | isStraight h && isFlush h -> StraightFlush ((last . concat) cs)
    cs | isStraight h -> Straight ((last . concat) cs)
    cs | isFlush h -> (Flush . reverse . concat) cs
    cs -> (HighCard . reverse . concat) cs

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)

isRoyalFlush :: Hand -> Bool
isRoyalFlush h = isFlush h && isStraight h && all (`elem` (fmap fst . getHand) h) [Ace, King]

isStraight :: Hand -> Bool
isStraight (Hand h)
    | (fst . head) hs == Two && (fst . last) hs == Ace =
        (diffIsOne . fmap (fromEnum . fst) . init) hs
    | otherwise = (diffIsOne . fmap (fromEnum . fst)) hs
        where
            hs = sort h

diffIsOne :: [Int] -> Bool
diffIsOne [_] = True
diffIsOne (x:y:rs) = y-x == 1 && diffIsOne (y:rs)

isFlush :: Hand -> Bool
isFlush (Hand ((_,c):cs)) = all ((==c) . snd) cs

