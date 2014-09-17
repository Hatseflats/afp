--2.5

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
--

--7.1
split [] = []
split (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- split xs]

perms [] = [[]]
perms xs = [(v:p) | (v, vs) <- split xs, p <- perms vs]

smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True

smoothPerms' :: Int -> [Int] -> [[Int]]
smoothPerms' n xs = filter (smooth n) (perms xs)

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n = concatMap pathLists . pruneTree n . buildTree

data Tree a = Tree a [Tree a]
    deriving (Show)

buildTree :: [a] -> [Tree a]
buildTree x = map tupleToTree (split x) 

tupleToTree :: (a,[a]) -> Tree a
tupleToTree (x,xs) = Tree x (buildTree xs)  

pathLists :: Tree a -> [[a]]
pathLists (Tree x []) = [[x]]
pathLists (Tree x xs) = map (x:) (concatMap pathLists xs)

pruneTree :: Int -> [Tree Int] -> [Tree Int]
pruneTree n = concatMap (pruneTree' n) where
    pruneTree' n (Tree x []) = [Tree x []]
    pruneTree' n (Tree x xs) = 
        case concatMap (pruneTree' n) (filter (checkDistance n x . root) xs) of
            [] -> []
            ys -> [Tree x ys]

    



checkDistance :: Int -> Int -> Int -> Bool
checkDistance distance x y = abs(x-y) <= distance

root :: Tree a -> a
root (Tree x _) = x

children :: Tree a -> [Tree a]
children (Tree _ xs) = xs

main = do
	print $ concatMap pathLists (pruneTree 3 (buildTree [2,3,5,6,8]))