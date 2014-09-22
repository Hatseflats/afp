p1, p2, p3 :: Int
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop

start = \c -> c []

store n = \xs c -> c (xs:n)

stop :: [Int] -> Int
stop (x:_) = x

add = \(x:y:xs) c -> c ([x+y] ++ xs)

mul = \(x:y:xs) c -> c ([x*y] ++ xs)

main = do
	print (p1)






