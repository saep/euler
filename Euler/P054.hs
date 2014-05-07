{- |
Module      :  Euler.P054
Description :  Problem 54
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Poker hands
Problem 54

In the card game poker, a hand consists of five cards and are ranked, from
lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest
value wins; for example, a pair of eights beats a pair of fives (see example 1
below). But if two ranks tie, for example, both players have a pair of queens,
then highest cards in each hand are compared (see example 4 below); if the
highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:
Hand	 	Player 1	 	Player 2	 	Winner
1	 	5H 5C 6S 7S KD
Pair of Fives
	 	2C 3S 8S 8D TD
Pair of Eights
	 	Player 2
2	 	5D 8C 9S JS AC
Highest card Ace
	 	2C 5C 7D 8S QH
Highest card Queen
	 	Player 1
3	 	2D 9C AS AH AC
Three Aces
	 	3D 6D 7D TD QD
Flush with Diamonds
	 	Player 2
4	 	4D 6S 9H QH QC
Pair of Queens
Highest card Nine
	 	3D 6D 7H QD QS
Pair of Queens
Highest card Seven
	 	Player 1
5	 	2H 2D 4C 4D 4S
Full House
With Three Fours
	 	3C 3D 3S 9S 9D
Full House
with Three Threes
	 	Player 1

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space): the
first five are Player 1's cards and the last five are Player 2's cards. You
can assume that all hands are valid (no invalid characters or repeated
cards), each player's hand is in no specific order, and in each hand there
is a clear winner.

How many hands does Player 1 win?

-}
module Euler.P054
       ( solve
       ) where

import Control.Arrow ((***))
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Maybe
import qualified Data.Map as M

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

solve :: IO ()
solve = do
    f <- readFile "text/poker.txt"
    let rawHands = fmap (parseHands . words) $ lines f
        hands = fmap (defineHandType *** defineHandType) rawHands
        p1wins = length $ filter (uncurry (>)) hands
    print p1wins

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

