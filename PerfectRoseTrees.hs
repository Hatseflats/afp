{-#LANGUAGE GADTs, FlexibleInstances, EmptyDataDecls#-}
module PerfectRoseTrees where

import Data.List
import Control.Applicative
import Data.Maybe

-- Integers on type level    
data O
data Succ a

-- | A data type for a rose tree in which all leaves have the same depth
data PerfectRoseTree' depth a where
    Leaf :: a ->                               PerfectRoseTree' O a
    Node :: a -> [PerfectRoseTree' depth a] -> PerfectRoseTree' (Succ depth) a

-- | A perfect rose tree where the type does not depend on the depth
data PerfectRoseTree a where
    PerfectRoseTree :: PerfectRoseTree' depth a -> PerfectRoseTree a

-- | A forest with perfect rose trees of the same depth
type PerfectRoseForest' depth a = [PerfectRoseTree' depth a]

-- | A perfect rose forest where the type does not depend on the depth
data PerfectRoseForest a where
    PerfectRoseForest :: PerfectRoseForest' depth a -> PerfectRoseForest a

leaf :: a -> PerfectRoseTree a
leaf = PerfectRoseTree . Leaf

leaf' :: a -> PerfectRoseTree' O a
leaf' = Leaf

root :: PerfectRoseTree a -> a
root (PerfectRoseTree t) = case t of
                              Leaf x   -> x
                              Node x _ -> x
                              
root' :: PerfectRoseTree' depth a -> a
root' = root . PerfectRoseTree

combine :: a -> PerfectRoseForest a -> PerfectRoseTree a
combine x (PerfectRoseForest ts) = PerfectRoseTree (Node x ts)

toForest :: PerfectRoseTree a -> PerfectRoseForest a
toForest (PerfectRoseTree t) = PerfectRoseForest [t]

paths' :: PerfectRoseTree' depth a -> [[a]]
paths' (Leaf x) = [[x]]
paths' (Node x ts) = map (x:) (ts >>= paths')

paths :: PerfectRoseTree a -> [[a]]
paths (PerfectRoseTree t) = paths' t

forestPaths' :: PerfectRoseForest' depth a -> [[a]]
forestPaths' ts = ts >>= paths'

forestPaths :: PerfectRoseForest a -> [[a]]
forestPaths (PerfectRoseForest ts) = forestPaths' ts