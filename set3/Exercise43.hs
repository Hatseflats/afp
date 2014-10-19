{-#LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances#-}

import Exercise42

--Eq instance--

eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True

eqCons :: (forall b. (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

{-
Without the forall it doesn't work, because the argument of eqT can then be any function that takes 2 arguments of the same type and returns a bool,
but eqT should work on any argument with type (a -> a -> Bool).
Without the forall, the programmer would have the freedom to choose the types a and b, but with the forall the programmer can only choose the type a,
and eqT must be a function of type (b -> b -> Bool) -> (t b -> t b -> Bool). Therefore, without the forall 'eqT eqA' doesn't have a valid type, but
with the forall it does.
-}

eqSquare' :: (forall b. (b -> b -> Bool) -> (t b -> t b -> Bool)) ->(a -> a -> Bool) -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA _         _         = False

{-
Without the forall you get a type error, for the same reason as in eqCons: eqT is applied to eqA, so 'a -> a -> Bool' has to match 'b -> b -> Bool'
but this can't be guaranteed if both a and b can be chosen freely by the programmer.
-}

eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
    (==) = eqSquare (==)
    
--Functor instance--

mapNil :: (a -> b) -> Nil a -> Nil b
mapNil mapA Nil = Nil

mapCons :: (forall c d. (c -> d) -> t c -> t d) -> (a -> b) -> Cons t a -> Cons t b
mapCons mapT mapA (Cons x xs) = Cons (mapA x) (mapT mapA xs)

mapSquare' :: (forall c d. (c -> d) -> t c -> t d) -> (a -> b) -> Square' t a -> Square' t b
mapSquare' mapT mapA (Zero xs) = Zero $ mapT (mapT mapA) xs
mapSquare' mapT mapA (Succ xs) = Succ $ mapSquare' (mapCons mapT) mapA xs

mapSquare :: (a -> b) -> Square a -> Square b
mapSquare = mapSquare' mapNil

instance Functor Square where
    fmap = mapSquare
    
Now let's test it on the squares from 4.2
test =
    do
        putStrLn "Equal to self:"
        print $ oneToNine == oneToNine && id2 == id2
        putStrLn "\r\nEqual to eachother:"
        print $ oneToNine == id2
        putStrLn "\r\n0 to 8:"
        print $ fmap (+ (-1)) oneToNine
        putStrLn "3x3 matrices equal:"
        print $ fmap (+ (-1)) oneToNine == oneToNine