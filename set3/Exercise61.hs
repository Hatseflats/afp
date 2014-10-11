{-#LANGUAGE GADTs, KindSignatures#-}

module Exercise61 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)

assert :: Contract a -> a -> a
assert (Pred p) x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre

assert' :: Contract a -> a -> a
assert' (Pred p) x = if p x then x else error "contract violation"
assert' (Fun pre post) f = assert' post . f . assert' pre

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

true :: Contract a
true = Pred (const True)

{-
Proof:
Let x be of any type
assert true x =                                                 by definition of true
assert (Pred (const True)) x =                                  by definition of assert
if ((const True) x) then x else error "contract violation" =    by definition of const
if True then x else error "contract violation" =                by definition of if .. then .. else ..
x
-}

func :: Contract (Integer -> Integer)
func = (Fun pos pos) 



main = do
    print $ assert func (\x -> x) 2