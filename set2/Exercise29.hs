--p1, p2, p3 :: Int
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
--p3 = start store 2 add stop

start = \c -> c []

store n = \l c -> c (l:n)

stop (x:xs) = x

add = \(s:ss:sss) c -> c ([s+ss] ++ sss)

mul = \(s:ss:sss) c -> c ([s*ss] ++ sss)

main = do
	print p2






