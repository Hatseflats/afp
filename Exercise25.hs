module Exercise25 where

test :: [Int]
test = [count, count 1 2 3, count "" [True, False] id (+)]

class Counter r where
	count' :: Int -> r

instance Counter Int where
	count' = id
    --count' = const 0

instance (Counter r) => Counter (a -> r) where
    count' acc = \x -> count' (acc+1)

count :: (Counter a) => a
count = count' 0
