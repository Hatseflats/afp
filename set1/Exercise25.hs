module Exercise25 where

test :: [Int]
test = [count, count 1 2 3, count "" [True, False] id (+)]

class Counter r where
	count' :: Int -> r

instance Counter Int where
	count' = id -- Always returns the number of arguments
    --count' = const 0 -- Always returns a zero, independent of the number of arguments

instance (Counter r) => Counter (a -> r) where
    count' acc = \x -> count' (acc+1)

count :: (Counter a) => a
count = count' 0
