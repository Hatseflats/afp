{-#LANGUAGE GADTs, KindSignatures#-}

module Exercise61 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
    List :: Contract a -> Contract [a]

assert :: Contract a -> a -> a
assert (Pred p) x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre
assert (DFun pre post) f = (\x -> assert (post x) (f (assert pre x)))
assert (List c) xs = map (assert c) xs

(-->) :: Contract a -> Contract b -> Contract (a -> b)
(-->) c c' = DFun c (\_ -> c')

index :: Contract ([a] -> Int -> a)
index = DFun true (\xs -> DFun true (\i -> Pred (\_ -> 0 <= i && i < length xs)))

preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true (\x -> Pred (\y -> f x == f y))

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

true :: Contract a
true = Pred (\_->True)

indexCheck1 = assert index (!!) [1..5] 2
indexCheck2 = assert index (!!) [0..7] 9

preserveCheck1 = assert (preserves length) reverse "Hello" == "olleH"
preserveCheck2 = assert (preserves length) (take 5) "Hello" == "Hello"
preserveCheck3 = assert (preserves length) (take 5) "Hello world" == undefined

main = do
    print $ preserveCheck3