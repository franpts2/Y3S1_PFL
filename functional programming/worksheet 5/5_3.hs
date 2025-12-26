import Data.List (sortBy) -- for testing

-- from 5_2.hs
data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show, Eq, Ord)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A
  deriving (Show, Eq, Ord)

data Card = Card Face Suit
  deriving (Show, Eq, Ord)

allCards :: [Card] -- for testing
allCards = [Card f s | s <- suits, f <- faces]
  where
    suits = [Clubs, Spades, Hearts, Diamonds]
    faces = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, J, Q, K, A]

-- a)
cmp1 :: Card -> Card -> Ordering
cmp1 (Card face1 suit1) (Card face2 suit2)
  | ((suit1 == suit2) && (face1 > face2)) || (suit1 > suit2) = GT
  | ((suit1 == suit2) && (face1 < face2)) || (suit1 < suit2) = LT
  | otherwise = EQ

-- b)
cmp2 :: Card -> Card -> Ordering
cmp2 (Card face1 suit1) (Card face2 suit2)
  | ((face1 == face2) && (suit1 > suit2)) || (face1 > face2) = GT
  | ((face1 == face2) && (suit1 < suit2)) || (face1 < face2) = LT
  | otherwise = EQ
