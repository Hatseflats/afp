{-#LANGUAGE GADTs#-}
module Assignment1 where
import Data.List
import PerfectRoseTrees
import Data.Maybe

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

prune :: Int -> PerfectRoseForest Int -> Maybe (PerfectRoseForest Int)
prune n (PerfectRoseForest []) = Just (PerfectRoseForest [])
prune n (PerfectRoseForest ts) = case map fromJust (filter isJust (map (prune' n) ts)) of
                                    [] -> Nothing
                                    ys -> Just (PerfectRoseForest ys)

prune' :: Int -> PerfectRoseTree' depth Int -> Maybe (PerfectRoseTree' depth Int)
prune' 0 (Node _ _)  = Nothing
prune' n (Leaf x)    = Just (Leaf x)
prune' n (Node x ts) = case fmap (prune' n) (filter (checkDistance n x . root') ts) of
                          []  -> Nothing
                          ts' -> Just (Node x (map fromJust (filter isJust ts')))
    where checkDistance distance x y = abs(x-y) <= distance

extract :: [a] -> [(a, [a])]
extract = extract' [] where
    extract' ys (x:xs) = (x, ys ++ xs) : extract' (ys ++ [x]) xs
    extract' _  _      = []
    
-- | Computes all smooth permutations of a list
smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n = maybePaths . prune n . getPermForest where
    maybePaths Nothing   = []
    maybePaths (Just ts) = forestPaths ts
