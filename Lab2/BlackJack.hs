module BlackJack where
import           Cards
import           RunGame
import           System.Random
import           Test.QuickCheck hiding (shuffle)

main :: IO ()
main = runGame implementation

-- A0
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
               Add (Card Jack Spades) Empty)

  = 1 + size (Add (Card Jack Spades) Empty)

  = 1 + 1 + size Empty

  = 2
-}

-- A1
empty :: Hand
empty = Empty

-- A2
-- Calculate value of rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10 -- _ is a face card

-- Calculate value of card by calculating value of its rank
valueCard :: Card -> Integer
valueCard card = valueRank $ rank card

-- Calculate value of hand based on BlackJack rules
-- Numeric n: Worth n
-- Face: Worth 10
-- Ace: Worth 11
initialValue :: Hand -> Integer
initialValue Empty     = 0
initialValue (Add c h) = valueCard c + initialValue h

-- Calculates number of aces in hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
  | rank card == Ace = 1 + numberOfAces hand
  | otherwise = numberOfAces hand

-- Calculate actual value of hand (Value of ace(s) can either be 11 or 1)
-- If hand is worth more than 21, subtract 10 for every ace present (11-10 = 1)
value :: Hand -> Integer
value hand
  | initialValue hand > 21 = initialValue hand - numberOfAces hand * 10
  | otherwise = initialValue hand

-- A3
-- Check if player is busted (hand is worth more than 21 points)
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4
-- Compares guest's hand to bank's hand and decides who won
-- Guest only wins if: Bank is bust or Guest's hand is worth more than the Bank's hand.
winner :: Hand -> Hand -> Player
winner gHand bHand
  | gameOver gHand = Bank
  | gameOver bHand = Guest
  | value gHand > value bHand = Guest
  | otherwise = Bank

-- B1

-- Operator puts one hand on top of another hand.
(<+) :: Hand -> Hand -> Hand
(<+) hand1 Empty       = hand1
(<+) Empty hand2       = hand2
(<+) (Add card1 hand1) hand2 = Add card1 newHand
  where newHand = hand1 <+ hand2

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)


-- B2

-- Function extracts a hand from a list of cards.
handFromList :: [Card] -> Hand
handFromList []           = Empty
handFromList [card]       = Add card Empty
handFromList (card:cards) = Add card (handFromList cards)

-- Function returns a hand of all ranks of a specific suit.
getSuitCards :: Suit -> Hand
getSuitCards suit = handFromList [Card (Numeric x) suit | x<-[2..10]] <+
                    Add (Card Jack suit) Empty <+
                    Add (Card Queen suit) Empty <+
                    Add (Card King suit) Empty <+
                    Add (Card Ace suit) Empty

-- Function returns a deck of all combinations of ranks and suits.
fullDeck :: Hand
fullDeck = getSuitCards Hearts <+
           getSuitCards Spades <+
           getSuitCards Clubs <+
           getSuitCards Diamonds

-- B3

-- Functions takes the top-most card from one hand, puts it on top of the
-- second hand and returns the two new hands as a tuple.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _              = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

-- B4

-- Function to play the bank with passed deck.
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Helper function for playBank. Takes a deck to draw cards from and
-- the bank's current hand as argument. Returns the bank's new hand when done
-- playing.
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand
   | value bankHand < 16 = playBank' deck' bankHand'
   | otherwise = bankHand
   where (deck', bankHand') = draw deck bankHand

-- B5

-- Given a hand, take a card at a random index and put it on top of new 'shuffled' deck.
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle generator hand = Add newCard Empty <+ shuffle newGenerator newHand
  where (index, g') = twoRandomIntegers generator hand
        (newCard, newHand) = taketh index hand
        newGenerator = mkStdGen (fromIntegral g')

-- Function which returns card at a certain index from passed hand.
-- Also returns new hand without said card.
taketh :: Integer -> Hand -> (Card, Hand)
taketh n _ | n < 0 = error "taketh: Index less than 0"
taketh 0 (Add card hand) = (card, hand)
taketh index (Add card hand) = (correctCard, Add card Empty <+ newHand)
  where (correctCard, newHand) = taketh (index-1) hand

-- Function given a hand and a stdgen returns a tuple with an index based on the hand and a seed for a new stdgen
twoRandomIntegers :: StdGen -> Hand -> (Integer,Integer)
twoRandomIntegers g hand = (n1, n2)
  where (n1, g1) = randomR (0, size hand - 1) g
        (n2, g2) = randomR (0, size hand - 1) g1


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Make sure that size of shuffled deck is preserved
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen hand = size hand == size (shuffle gen hand)

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }
