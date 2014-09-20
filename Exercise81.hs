module Exercise81 where

import Exercise71
import Exercise25

import Criterion
import Test.QuickCheck

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

checkPerms :: IO ()
checkPerms = quickCheck testPerms

permBenchmarks :: [Benchmark]
permBenchmarks = [
                    bench "naive" $ nf (allSmoothPerms testn) testdata,
                    bench "pro" $ nf (smoothPerms testn) testdata
                ] where testdata = [2,3,5,6,7,8,9]
                        testn = 3

--main = putStr . show $ deepseq ((smoothPerms 5 [2,3,5,6,7,8,10,12,15,16,17])) "blah" 
--main = return(deepseq (allSmoothPerms 15 [2,3,5,6,7,8,9,10,11,12,15,16,17]) ())
main = do
	print test