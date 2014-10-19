{-#LANGUAGE GADTs, KindSignatures#-}

module Exercise61 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
    List :: Contract a -> Contract [a]

assert :: Contract a -> a -> a
assert (Pred p) x = if p x then x else error "contract violation"
assert (DFun pre post) f = (\x -> assert (post x) (f (assert pre x)))
assert (List c) xs = map (assert c) xs

-- | Replaces the old Fun constructor
(-->) :: Contract a -> Contract b -> Contract (a -> b)
(-->) c c' = DFun c (\_ -> c')

index :: Contract ([a] -> Int -> a)
index = DFun true (\xs -> DFun true (\i -> Pred (\_ -> 0 <= i && i < length xs)))

preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true (\x -> Pred (\y -> f x == f y))

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

true :: Contract a
true = Pred (const True)
{-
For any x, assert true x = x

Proof:
Let x be of any type
assert true x =                                                 by definition of true
assert (Pred (const True)) x =                                  by definition of assert
if ((const True) x) then x else error "contract violation" =    by definition of const
if True then x else error "contract violation" =                by definition of if .. then .. else ..
x
                                                                Q.E.D.
-}

preservesPos = preserves (>0)
preservesPos' = pos --> pos

{-
preservesPos will only fail if the result of (>0) differs between the pre check and the post check.
preservesPos' will fail when the value isn't positive.

	assert preservesPos (+1) (-3) == -2

Will not fail despite the the result being negative.

	assert preservesPos' (+1) (-3) == -2

This will fail.
-}

allPos = List pos
allPos' = Pred (all (>0))

{-
The difference between the two versions of allPos is that allPos is lazy due to the use of map.
However, allPos' isn't lazy because of the all function. 
This means that: 

	length (assert allPos [1,2,-3])

Will return 3. But the following will fail:

	length (assert allPos' [1,2,-3])

Since all directly evaluates the values in the list, this means that for an infinite list the following won't finish:
	
	assert allPos' [1..]
-}

indexCheck1 = assert index (!!) [1..5] 2
indexCheck2 = assert index (!!) [0..7] 9

preserveCheck1 = assert (preserves length) reverse "Hello" == "olleH"
preserveCheck2 = assert (preserves length) (take 5) "Hello" == "Hello"
preserveCheck3 = assert (preserves length) (take 5) "Hello world" == undefined