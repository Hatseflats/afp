{-#LANGUAGE GADTs, KindSignatures#-}

module Exercise61 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)

assert :: Contract a -> a -> a
assert (Pred p) x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre
assert (DFun pre post) f = (\x -> assert (post x) (f (assert pre x)))

(-->) :: Contract a -> Contract b -> Contract (a -> b)
(-->) c c' = DFun c (\_ -> c')

index :: Contract ([a] -> Int -> a)
index = DFun true (\xs -> DFun true (\i -> Pred (\_ -> 0 <= i && i < length xs)))

indexCheck = assert index (!!) [1..5] 2

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

true :: Contract a
true = Pred (\_->True)

main = do
    print $ indexCheck