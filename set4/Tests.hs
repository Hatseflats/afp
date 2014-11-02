{-#LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable, TemplateHaskell#-}

module Tests where

import Test.QuickCheck
import Exercise111
import Data.Data
import Control.Monad

data Tree a = Leaf a | Node (Tree a) (Tree a) 
    deriving (Show, Read, Typeable, Data, Eq)

instance Arbitrary (Tree Int) where
    arbitrary = frequency 
        [ (5, liftM Leaf (choose (0,20)))
        , (1, liftM2 Node arbitrary arbitrary)]


-- You can't check polymorphic properties, so define a lot of tests for standard types

prop_showInt :: Int -> Bool 
prop_showInt x = (show x) == (show' x)

prop_showBool :: Bool -> Bool 
prop_showBool x = (show x) == (show' x)

prop_showChar :: Char -> Bool 
prop_showChar x = (show x) == (show' x)

prop_showIntTup :: (Int,Int) -> Bool 
prop_showIntTup x = (show x) == (show' x)

prop_showIntList :: [Int] -> Bool 
prop_showIntList x = (show x) == (show' x)

prop_showTree :: Tree Int -> Bool
prop_showTree x = (show x) == (show' x)

prop_readInt :: Int -> Bool
prop_readInt x = (read x'::Int) == (read' x'::Int)
    where x' = show x

prop_readBool :: Bool -> Bool
prop_readBool x = (read x'::Bool) == (read' x'::Bool)
    where x' = show x

prop_readChar :: Char -> Bool
prop_readChar x = (read x'::Char) == (read' x'::Char)
    where x' = show x

prop_readIntTup :: (Int,Int) -> Bool
prop_readIntTup x = (read x'::(Int,Int)) == (read' x'::(Int,Int))
    where x' = show x

prop_readIntList :: [Int] -> Bool
prop_readIntList x = (read x'::[Int]) == (read' x'::[Int])
    where x' = show x

prop_readTree :: Tree Int -> Bool
prop_readTree x = (read x'::Tree Int) == (read' x'::Tree Int)
    where x' = show x

return []
runTests = $quickCheckAll

main = do
    runTests

