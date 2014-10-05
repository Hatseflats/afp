{-#LANGUAGE GADTs#-}

module AlternativeSquares where

data O
data Succ n

-- | Data type for a list with the length as phantom type
data ListWithLength l a where
    Empty ::                            ListWithLength O a
    (:+:) :: a -> ListWithLength l a -> ListWithLength (Succ l) a

infixr 5 :+:

type SquareWithSize l a = ListWithLength l (ListWithLength l a)

data Square a where 
    Square :: SquareWithSize l a -> Square a
    
--The 2x2 identity matrix
id2 :: Square Int
id2 = Square $ (1 :+: 0 :+: Empty) :+: (0 :+: 1 :+: Empty) :+: Empty

--The 3x3 matrix containing the numbers 1 to 9
oneToNine :: Square Int
oneToNine = Square $ (1 :+: 2 :+: 3 :+: Empty) :+: (4 :+: 5 :+: 6 :+: Empty) :+: (7 :+: 8 :+: 9 :+: Empty) :+: Empty

toList :: ListWithLength l a -> [a]
toList Empty = []
toList (x :+: xs) = x : toList xs

instance Show a => Show (ListWithLength l a) where
    show = show . toList
    
instance Show a => Show (Square a) where
    show (Square sq) = unlines . map (unwords . map show . toList) . toList $ sq