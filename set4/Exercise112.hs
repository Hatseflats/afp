{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE TypeOperators#-}

module Exercise122 where

import GHC.Generics

class GRead a where
    greadsPrec :: Int -> String -> [(a,String)]

instance GRead Char   where greadsPrec = readsPrec
instance GRead Int    where greadsPrec = readsPrec
instance GRead Float  where greadsPrec = readsPrec
instance GRead String where greadsPrec = readsPrec
instance GRead Bool   where greadsPrec = readsPrec

--Some simple type to test with
data T a = T a deriving (Show, Read, Generic)

class GRead' f where
    greadsPrec' :: Int -> String -> [(f a, String)]

instance GRead' U1 where
    greadsPrec' _ s = [(U1, s)]

instance (GRead c) => GRead' (K1 i c) where
    greadsPrec' n x = [(K1 (fst x'), snd x')]
        where (x':_) = greadsPrec n x

instance (GRead' a) => GRead' (M1 i d a) where
    greadsPrec' n x = [(M1 (fst x'), snd x')]
        where (x':_) = greadsPrec' n x

instance (GRead' a, GRead' b) => GRead' (a :*: b) where
    greadsPrec' n x = [(a :*: b, x'')]
        where   ((a, x'): _) = greadsPrec' n x
                ((b, x''): _) = greadsPrec' n x'

greadsPrecDefault :: (Generic a, GRead' (Rep a)) => Int -> String -> [(a,String)]
greadsPrecDefault n x = [(to (fst rep), snd rep)]
    where (rep:_) = (greadsPrec' n x)

main = do
    print $ (greadsPrecDefault 0 "1" :: [(Int, String)])
    print $ (greadsPrecDefault 0 "'a'" :: [(Char, String)])
    print $ (greadsPrecDefault 0 "'a'" :: [(String, String)])
    --print $ (greadsPrecDefault 0 "T 1" :: [(T Int,String)])
    --print $ (greadsPrecDefault 0 "(1,1)" :: [((Int,Int), String)])
    --print $ (greadsPrecDefault 0 "True" :: [(Bool,String)])

