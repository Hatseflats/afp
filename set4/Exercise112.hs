{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercise122 where

import GHC.Generics

gread :: GRead a => String -> a
gread s = case greadsPrec 0 s of
             [(x, "")] -> x
             _         -> error "No valid parse"

class GRead a where
    greadsPrec :: Int -> String -> [(a,String)]

instance GRead Char   where greadsPrec = readsPrec
instance GRead Int    where greadsPrec = readsPrec
instance GRead Float  where greadsPrec = readsPrec
instance GRead String where greadsPrec = readsPrec
instance GRead Bool   where greadsPrec = readsPrec

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

greadsPrecDefault :: (Generic a, GRead' (Rep a)) => Int -> String -> [(a,String)]
greadsPrecDefault n x = [(to (fst rep), snd rep)]
    where (rep:_) = (greadsPrec' n x)

main = do
    print $ (greadsPrecDefault 0 "55 5" :: [(Int,String)])
    --print $ lex "[1,2,3]" 
