{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Prac3 where

import System.Random
import Control.Monad.Reader
{- Exercise 4.3 -}
type Square = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a = Nil
data Cons t a = Cons a (t a)

eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True

eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) -> (a -> a -> Bool) -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA _ _ = False

eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
    (==) = eqSquare (==)

{-Starting the real exercise
The forall in the type signature forces the function to be polymorphic, which means the callee (eqCons) can use it on anything.
With eqCons however it is not needed since it also works with universal polymorphism.
In the eqSquare' it is needed because in the Zero case eqT is applied twice in eqT (eqT eqA) the eqT eqA returns another type than eqA is on its own.
So eqT must be polymorphic inside eqSquare' so the rankN quantifier is needed to make sure this is possible. The program will not type check if you remove it
-}
-- | map base case
mapNil :: (a -> b) -> Nil a -> Nil b    
mapNil _ Nil = Nil

-- | map version for Cons applying the function and lifting it for the recursion
mapCons :: (forall b . (b -> c) -> (t b -> t c)) -> (b -> c) -> (Cons t b -> Cons t c)
mapCons fa fb (Cons x xs) = (Cons (fb x) (fa fb xs)) 

-- | Map version for Square' reconstructs a Square' and applies the function recursively 
mapSquare' :: (forall c d . (c -> d) -> (t c -> t d)) -> (a -> b) -> (Square' t a -> Square' t b)
mapSquare' fa fb (Zero xs) = (Zero (fa (fa fb) xs))
mapSquare' fa fb (Succ xs) = (Succ (mapSquare' (mapCons fa) fb xs)) 

-- | Map for the Square initialized with the base map
mapSquare :: (a -> b) -> Square a ->Square b
mapSquare = mapSquare' mapNil

instance Functor Square where
    fmap = mapSquare


--Testing squares    
sq1, sq2 :: Square' Nil Int
sq1 = Succ (Succ (Zero (Cons(Cons 1(Cons 2 Nil )) ((Cons (Cons 3(Cons 4 Nil))Nil)))))
sq2 = Succ (Succ (Zero (Cons(Cons 2(Cons 3 Nil )) ((Cons (Cons 4(Cons 5 Nil))Nil)))))
--This test should and does return [True, False, True]
squaretest :: [Bool]
squaretest = [sq1 == sq1, sq1 == sq2,(fmap (+1) sq1) == sq2]

{- Exercise 5.2 -}
--See PDF
{- Exercise 5.3 -}
one :: Int
one = 1
two :: Int
two = 2
randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n
{-
sizedInt = do
           n <- ask
           g <- lift ask
           return (randomN n g)
           -}
{- Exercise 6.1 -}
data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
    List :: Contract a -> Contract [a]
    
assert :: Contract a -> a -> a
assert (Pred p)       x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . (f . assert pre)
assert (DFun pre post) f = \x -> assert (post x) . (f . assert pre) $ x
assert (List c) xs = map (assert c) xs

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

true :: Contract a
true = Pred (\_ -> True)

(-->) :: Contract a -> Contract b -> Contract (a -> b)
(-->) a b = DFun a (\_ -> b)

listindex :: Contract ([a] -> Int -> a)
listindex = DFun true (\xs -> DFun (indexContract xs) (\_ -> true)) where
    indexContract xs = Pred (\n -> n >= 0 && n < length xs)

-- | preserves does not check any thing pre function, it only checks after wards of the same result holds on input and output    
preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun (Pred (\_->True)) (\pre -> (Pred (\post -> (f pre) == (f post))))

preservesPos, preservesPos':: Contract (Integer -> Integer)
preservesPos = preserves (>0)
preservesPos' = pos --> pos

allPos, allPos' :: Contract [Integer]
allPos = List pos
allPos' = Pred (all (>0))
{- 
preservesPos only checks whether the pre and post are equal. while preservesPos requires them both to be positive.
A counter example would be:
assert preservesPos id 0    this gives 0, both pre and post are negative but comparing they are the same and the property is preserved though its false.
assert preservesPos' id 0   this gives an error because neither is positive.

List vs. Pred
If you use List you ensure you check each element on the list given a predicate, 
if you use Pred this can be forgotten which can result in checking a property of the list structure rather than the elements.

List can give back a sublist of an infite list
Pred however needs to check all before it can return a part of the list because "all" only can return true on finite lists.
-}
{- Exercise 8.4 -}
forceBoolList :: [Bool] -> r -> r
forceBoolList [] r = r
forceBoolList (True:xs) r = forceBoolList xs r
forceBoolList (False:xs) r = forceBoolList xs r
{-
with [Bool] -> [Bool] there would be no need to execute the function so nothing would be evaluated.
seq a b returns the value of b, but makes that value depend on the evaluation of a.

force a = seq a a is useless because it would do nothing. The value of the second "a" becomes dependend on the first,
but since it is actually the same value it does not change. It would be shorter to just write "a" it would have the same meaning.
-}
{- Exercise 8.5 -}
-- We could define this endlessly...
data D1 = D1 D2
data D2 = D2 D3
data D3 = D4 String
