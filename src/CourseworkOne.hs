{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module CourseworkOne where

import Halatro.Constants
import Halatro.Types
import Data.List
import qualified Data.Type.Bool as Straight
import qualified Control.Applicative as TwoPairs

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- helper 1
-- Calculates occurences of a given element in a given list
calculateOccurences :: Eq a => [a] -> a -> Int
calculateOccurences list el = length (filter (el ==) list)

-- helper 2
-- Sorting is necessary before grouping (so that cards with the same rank are next to each other) as otherwise grouping will not work as intended
rankOccurences :: [Card] -> [(Rank, Int)]
rankOccurences cards = [ (head l, length l) | l <- group $ sort $ map rank cards ]

-- helper 3
-- Sorting is necessary before grouping (so that cards with the same rank are next to each other) as otherwise grouping will not work as intended
suitOccurences :: [Card] -> [(Suit, Int)]
suitOccurences cards = [ (head l, length l) | l <- group $ sort $ map suit cards ]



-- General note 1. I sometimes use dollar and sometimes parentheses. The reason I do not use dollar always is because in some functions 
-- it is much easier to read them when used with parentheses.

-- General note 2. I added a lot of additional tests in a Spec.hs file so as to cover as many boundary cases as possible. 
-- They actually helped me to find several errors in Ex.1


contains :: Hand -> HandType -> Bool

contains [] None = True
contains [] _ = False
contains _ None = False

-- It is important not only to ensure that each rank occurs only once, but also that it is not a Flush or Straight,
-- since they can contain all cards of different ranks.
contains cards HighCard = all ((==1) . snd) (rankOccurences cards) && not (contains cards Flush) && not (contains cards Straight)


-- Need a pair where the 2nd entry is >= 2, which means that a particular rank occured twice
contains cards Pair = any ((>=2) . snd) $ rankOccurences cards


-- Need 2 pairs where the 2nd entry is >= 2, which means that we have two pairs. 
-- Since FourOfAKind is also a TwoPair, we separately check for that as well.
contains cards TwoPair = length (filter ((>=2)  . snd) $ rankOccurences cards) >= 2 || contains cards FourOfAKind


-- Need a pair where the 2nd entry is >= 3, which means that a particular rank occured three times.
contains cards ThreeOfAKind = any ((>=3) . snd) $ rankOccurences cards


-- We know there are 5 elements in a played hand, thus we take the first and last elements of a hand passed in
-- and enumerate between them. If the enumeration matches the hand being passed in, then we have got a Straight.
-- We also check for special case with Ace.
contains cards Straight
    | length sortedRanks /= 5 = False
    | sortedRanks == case1 = True
    | sortedRanks == [Two, Three, Four, Five, Ace] = True
    | otherwise = False
    where
        sortedRanks = sort $ map rank cards
        case1 = enumFromTo (head sortedRanks) (last sortedRanks)



-- Need one pair where the 2nd entry is >= 5, which means that a particular suit occured 5 times.
contains cards Flush = any ( (>=5)  . snd) $ suitOccurences cards

-- Using where clause so as to avoid computing the function rankOccurences twice
contains cards FullHouse = any (\x -> snd x == 3) rankOccurencesComputed && any (\x -> snd x == 2) rankOccurencesComputed
    where
        rankOccurencesComputed = rankOccurences cards


-- Need a pair where the 2nd entry is >= 4, which means that a particular rank occured four times.
contains cards FourOfAKind = any ((>=4) . snd) $ rankOccurences cards



-- Must satisfy Straight and Flush
contains cards StraightFlush = contains cards Straight && contains cards Flush



-- Must be a particular combination of ranks and must be a Flush (i.e. all of the same suit)
contains cards RoyalFlush = checkList == sortedRanks && contains cards Flush
    where
        sortedRanks = sort $ map rank cards
        checkList = [Ten, Jack, Queen, King, Ace]





--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

bestHandType :: Hand -> HandType
bestHandType [] = None
bestHandType hand
    | contains hand RoyalFlush = RoyalFlush
    | contains hand StraightFlush = StraightFlush
    | contains hand FourOfAKind = FourOfAKind
    | contains hand FullHouse = FullHouse
    | contains hand Flush = Flush
    | contains hand Straight = Straight
    | contains hand ThreeOfAKind = ThreeOfAKind
    | contains hand TwoPair = TwoPair
    | contains hand Pair = Pair
    | contains hand HighCard = HighCard





--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Whenever you see "map fst ....." below, we zip the hand with occurences of each rank, e.g.
-- hand = [ (Card Two Clubs), (Card Three Clubs), (Card Two Diamonds), (Card Jack Hearts), (Card Jack Clubs) ]
-- occurences = [ 2, 1, 2, 2, 2 ]
-- Hence, zip hand occurences result in [ ((Card Two Clubs), 2), ((Card Three Clubs), 1), ((Card Two Diamonds), 2), ... ]
-- Note that the 2nd element of a pair is the number of occurences of that particular RANK, NOT CARD !

whichCardsScore :: Hand -> [Card]

whichCardsScore hand =
    let
        bestHT = bestHandType hand
        ranks = map rank hand
        occurences = map (calculateOccurences ranks) ranks
    in
        case bestHT of
        RoyalFlush -> hand
        StraightFlush -> hand
        FourOfAKind -> map fst (filter ((==) 4 . snd) $ zip hand occurences)
        FullHouse -> hand
        Flush -> hand
        Straight -> hand
        ThreeOfAKind -> map fst (filter ((==) 3 . snd) $ zip hand occurences)
        TwoPair -> map fst (filter ((==) 2 . snd) (zip hand occurences))
        Pair -> map fst (filter ((==) 2 . snd) (zip hand occurences))
        HighCard -> [maximum hand]
        None -> []




-- Applying right fold (recursion under the hood) to sum up the score for each rank, then add the base chips and finally multiply by mult
scoreHand :: Hand -> Int
scoreHand [] = 0
scoreHand hand = (chips + foldr ( \x y -> rankScore x + y ) 0 ranks) * mult
    where
        (chips, mult) = handTypeValues $ bestHandType hand
        ranks = map rank $ whichCardsScore hand






--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

-- Subsequences is like generating subsets in Maths, i.e. it does not allow for duplicates.
-- List comprehension could be used instead , but that would require a lot of additional conditional statements to ensure that
-- the x1, x2, x3, x4, x5 are all different, otherwise the result is incorrect.

-- I deliberately put the score as the first element of a tuple, because Haskell applies lexicographic ordering for pairs,
-- thus, I can just call maximum function and not bother with other functions which explicitly tell Haskell by which element to sort tuples.
highestScoringHand :: [Card] -> Hand
highestScoringHand [] = []
highestScoringHand hand = whichCardsScore playedHand
    where
        combination = filter (\x -> length x <= 5) $ subsequences hand
        scoresCombination = map (\x -> (scoreHand x, x)) combination
        (score, playedHand) = maximum scoresCombination




--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI prevMoves cards = Move Play $ take 5 $ reverse $ sort cards



sensibleAI :: [Move] -> [Card] -> Move
sensibleAI prevMoves cards = Move Play $ highestScoringHand cards



--Average 650-670

-- We generally discard only the cards which did not score for a particular combination.
-- This is essential, because discarding all cards will likely lead to a worse combination.

-- Discarding High card and Pairs will obviously lead to higher score. So will discarding TwoPairs.
-- Testing helped to pinpoint to me that actually discarding ThreeOfAKind leads on average to a better score (+10 approx.)

myAI :: [Move] -> [Card] -> Move
myAI prevMoves cards =  case bestHT of
        HighCard 
            | numberOfDiscards < 3 -> Move Discard (take 5 sortedCards)
            | otherwise -> Move Play whichCardsScored
                where 
                    sortedCards = sort whichCardsDoNotScore
        Pair 
            | numberOfDiscards < 3 -> Move Discard (take 5 sortedCards)
            | otherwise -> Move Play whichCardsScored
                where
                    sortedCards = sort whichCardsDoNotScore
        TwoPair 
            | numberOfDiscards < 3 ->  Move Discard whichCardsDoNotScore 
            | otherwise -> Move Play whichCardsScored
        ThreeOfAKind 
            | numberOfDiscards < 3 -> Move Discard whichCardsDoNotScore 
            | otherwise -> Move Play whichCardsScored
        
        Straight -> Move Play whichCardsScored
        Flush -> Move Play whichCardsScored
        FullHouse -> Move Play whichCardsScored
        
        FourOfAKind -> Move Play whichCardsScored
        StraightFlush -> Move Play whichCardsScored
        RoyalFlush -> Move Play whichCardsScored
        None -> Move Play []
        
        where 
            whichCardsScored = highestScoringHand cards
            whichCardsDoNotScore = cards \\ whichCardsScored 
            bestHT = bestHandType whichCardsScored

            numberOfDiscards = length $ filter 
                (\x -> 
                    case x of 
                        (Move Discard _) -> True
                        _ -> False
                ) 
                prevMoves




            findDiscard (Move Discard _) = True

            noOfDiscards = filter findDiscard 

            number lis = length ( noOfDiscards lis)
