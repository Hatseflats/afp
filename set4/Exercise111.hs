{-#LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable#-}
module Exercise111 where

import Data.List
import Data.Data

class Show' a where
    show' :: a -> String
    showsPrec' :: Int -> a -> ShowS
    show' x = showsPrec' 0 x ""
    showsPrec' _ x s = show' x ++ s
    
class Read' a where
    read' :: String -> a
   
   {-
   Overlapping instances doesn't seem to work well with rank N types: passing show' to gmapQ results in an error if there are overlapping instances for Show'.
   
instance Show' Bool where showsPrec' = showsPrec
instance Show' Int where showsPrec' = showsPrec
instance Show' Integer where showsPrec' = showsPrec
instance Show' Float where showsPrec' = showsPrec
instance Show' Double where showsPrec' = showsPrec
instance Show' Char where showsPrec' = showsPrec
instance Show' [Char] where showsPrec' = showsPrec
    
-- | Strings have a special representation
instance Show' a => Show' [a] where
    showsPrec' _ xs = showChar '[' . foldr (.) id (intersperse (showChar ',') (map (showsPrec' 0) xs)) . showChar ']'
    
-- | Tuples also have a special representation, we just implement the case for the binary tuple
instance (Show' a, Show' b) => Show' (a, b) where
    showsPrec' _ (x, y) = showChar '(' . showsPrec' 0 x . showChar ',' . showsPrec' 0 y . showChar ')'
    -}
    
-- | Making Data instance of Show'
-- Precedence and associativity is not supported by Data, so unfortunately we need to include parantheses whenever we use infix operators (unless we only use one).
instance Data a => Show' a where
    showsPrec' n x | isAlgType (dataTypeOf x) && constrFixity c == Infix = showParen (n > 0) $ ((inf1 . showString (showInfConstr c) . inf2))
                   | otherwise = showParen (n > 10 && hasFields c) $ showString (showConstr c) . foldr (\f g -> showChar ' ' . f . g) (showString "") (gmapQ (showsPrec' 11) x)
        where c = toConstr x
              showParen b f = if b then showChar '(' . f . showChar ')' else f
              --hasFields assumes primitive types don't have fields
              hasFields = if isAlgType (dataTypeOf x) then not . null . constrFields else const False
              [inf1, inf2] = take 2 (gmapQ (showsPrec' 11) x)
              showInfConstr c = let s = showConstr c in take (length s - 2) (drop 1 s)
    
--Some simple type to test with
data T a b = T a b deriving (Show, Typeable, Data)