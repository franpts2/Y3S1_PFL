data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Show)
data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A
    deriving (Show)

data Card = Card Face Suit
    deriving (Show)

allCards :: [Card]
allCards = [Card f s  | s <- suits, f <- faces]
    where 
        suits = [Clubs,Spades,Hearts,Diamonds]
        faces = [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,J,Q,K,A]

