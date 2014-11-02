{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercise122 where

import GHC.Generics

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

greadsPrecDefault :: (Generic a, GRead' (Rep a)) => Int -> String -> [(a,String)]
greadsPrecDefault n x = to (fst rep)
    where (rep:_) = (greadsPrec' n x)

main = do
    --print $ (greadsPrec' 0 "5" :: [(Int,String)])
    print $ "t"
