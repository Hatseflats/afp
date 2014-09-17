import Assignment1
import Control.DeepSeq
import Data.List

split [] = []
split (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- split xs]

perms' [] = [[]]
perms' xs = [(v:p) | (v, vs) <- split xs, p <- perms' vs]

smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True

allSmoothPerms :: Int -> [Int] -> [[Int]]
allSmoothPerms n xs = filter (smooth n) (perms' xs)

testPerms :: Int -> [Int] -> Bool
testPerms n xs = allSmoothPerms n xs' == smoothPerms n xs'
    where xs' = take 4 xs

--checkPerms :: IO ()
--checkPerms = quickCheck testPerms

--permBenchmarks :: [Benchmark]
--permBenchmarks = [
--                    bench "naive" $ nf (allSmoothPerms testn) testdata,
--                    bench "pro" $ nf (smoothPerms testn) testdata
--                ] where testdata = [2,3,5,6,7,8,9]
--                        testn = 3

main = putStr . show $ deepseq ((smoothPerms 5 [2,3,5,6,7,8,10,12,15,16,17])) "blah" 
--main = return(deepseq (allSmoothPerms 15 [2,3,5,6,7,8,9,10,11,12,15,16,17]) ())

--we first prove the following lemma:
For all (xs :: [a]) (ys :: [a]), xs is either [] or (x:xs') for some (xs' :: [a]) (this follows from the definition of lists)
For the case xs = [] we have: 
	length ([] ++ ys) = length ys = --0 is the identity element of addition
	0 + length ys = --from the definition of length
	length [] + length ys
For the case xs = (x:xs'), assume the lemma holds for xs', we have: 
	length ((x:xs') ++ ys) = --from the definition of ++
	length (x:(xs' ++ ys)) = --from the definition of length
	1 + length (xs' ++ ys) = --by the induction hypothesis
	1 + length xs' + length ys = --from the definition of length
	length (x:xs') + length ys
--QED

--we will now prove the theorem using induction on trees
For all (x :: a) we have:
	length (flatten (Leaf x)) = --by the definition of flatten
	length [x] = --by the definition of length
	1 + (length []) = --by the definition of length
	1 + 0 = 1 = --by the definition of size
	size (Leaf x)
For all (l :: Tree a) (r :: Tree a), assuming the theorem holds for l and r, we have:
	length (flatten (Node l r)) = --by the definition of flatten
	length (flatten l ++ flatten r) = --by the lemma
	length (flatten l) ++ length (flatten r) = --by the induction hypothese
	size l + size r = --by the definition of size
	size (Node l r)
--QED
