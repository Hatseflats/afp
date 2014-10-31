{-#LANGUAGE OverlappingInstances, FlexibleInstances#-}
module Exercise111 where

import Data.Generics
import Data.List

class Show' a where
    show' :: a -> String
    showsPrec' :: Int -> a -> ShowS
    show' x = showsPrec' 0 x ""
    showsPrec' _ x s = show' x ++ s
    
class Read' a where
    read' :: String -> a
   
instance Show' Bool where showsPrec' = showsPrec
instance Show' Int where showsPrec' = showsPrec
instance Show' Integer where showsPrec' = showsPrec
instance Show' Float where showsPrec' = showsPrec
instance Show' Double where showsPrec' = showsPrec
instance Show' Char where showsPrec' = showsPrec
instance Show' [Char] where showsPrec' = showsPrec
    
instance Show' a => Show' [a] where
    showsPrec' _ xs = showChar '[' . foldr (.) id (intersperse (showChar ',') (map (showsPrec' 0) xs)) . showChar ']'