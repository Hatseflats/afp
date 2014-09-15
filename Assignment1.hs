{-#LANGUAGE GADTs#-}
module Ass1 where
import Data.List
import PerfectRoseTrees

-- | Creates a PerfectRoseForest from a list with the property that there is a one-to-one correspondence 
--   between the permutations of the list and the paths from a root to a leaf in the forest
getPermForest :: [a] -> PerfectRoseForest a
getPermForest [] = PerfectRoseForest []
getPermForest (x:xs) = insert x (getPermForest xs) where

    insert :: a -> PerfectRoseForest a -> PerfectRoseForest a
    insert x (PerfectRoseForest []) = PerfectRoseForest [leaf' x]
    insert x (PerfectRoseForest ts) = PerfectRoseForest (Node x ts : (ts >>= insert' x))
    
    insert' :: a -> PerfectRoseTree' depth a -> PerfectRoseForest' (Succ depth) a
    insert' x (Leaf y) = [Node y [leaf' x]]
    insert' x (Node y ts) = [Node y (Node x ts : concatMap (insert' x) ts)]
 
-- | Computes all permutations of a list
perms :: [a] -> [[a]]
perms = forestPaths . getPermForest

prune :: Int -> PerfectRoseForest Int -> PerfectRoseForest Int
prune n (PerfectRoseForest ts) = PerfectRoseForest $ concatMap (prune' n) ts

prune' :: Int -> PerfectRoseTree' depth Int -> PerfectRoseForest' depth Int
prune' n (Leaf x)    = [Leaf x]
prune' n (Node x ts) = case concatMap (prune' n) (filter (checkDistance n x . root') ts) of
                          [] -> []
                          ts' -> [Node x ts']
    where checkDistance distance x y = abs(x-y) <= distance

extract :: [a] -> [(a, [a])]
extract = extract' [] where
    extract' ys (x:xs) = (x, ys ++ xs) : extract' (ys ++ [x]) xs
    extract' _  _      = []
    
-- | Computes all smooth permutations of a list
smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n = forestPaths . prune n . getPermForest
    
perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' xs = [(y:p) | (y, ys) <- extract xs, p <- perms' ys]