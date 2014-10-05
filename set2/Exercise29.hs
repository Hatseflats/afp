{-#LANGUAGE GADTs#-}

module Exercise29 where

p1, p2 :: Int
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
--p3 has no valid type
--p3 = start store 2 add stop

data O
data Succ n

-- | Data type for a list with the length as phantom type
data ListWithLength l a where
    Empty ::                            ListWithLength O a
    (:+:) :: a -> ListWithLength l a -> ListWithLength (Succ l) a

infixr 5 :+:
    
start :: (ListWithLength O a -> b) -> b
start c = c Empty

store :: (ListWithLength l a) -> a -> ((ListWithLength (Succ l) a) -> b) -> b
store xs x c = c (x:+:xs)

stop :: (ListWithLength (Succ l) a) -> a
stop (x:+:_) = x

add :: Num a => (ListWithLength (Succ (Succ l)) a) -> ((ListWithLength (Succ l) a) -> b) -> b
add (x:+:y:+:xs) c = c ((x+y) :+: xs)

mul :: Num a => (ListWithLength (Succ (Succ l)) a) -> ((ListWithLength (Succ l) a) -> b) -> b
mul (x:+:y:+:xs) c = c ((x*y) :+: xs)

--main = do
--	print (p1)






