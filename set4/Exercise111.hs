{-#LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable#-}
module Exercise111 where

import Data.List
import Data.Data
import Data.Generics.Aliases
import Text.ParserCombinators.ReadP
import Control.Monad

class Show' a where
    show' :: a -> String
    showsPrec' :: Int -> a -> ShowS
    show' x = showsPrec' 0 x ""
    showsPrec' _ x s = show' x ++ s
    
class Read' a where
    readsPrec' :: Int -> ReadS a
    
read' :: Read' a => String -> a
read' s = case readsPrec' 0 s of
             [(x, "")] -> x
             _         -> error "No valid parse"
    
{-
Overlapping instances doesn't seem to work well with rank N types: passing show' to gmapQ results in an error if there are overlapping instances for Show'.
Instead, we include the special cases in the Show' instance definition of Data, using the DataRep representation.
-}
    
-- | Making Data instance of Show'
-- Precedence and associativity is not supported by Data, so unfortunately we need to include parantheses whenever we use infix operators (unless we only use one).
instance Data a => Show' a where
    showsPrec' n x --Special cases:
                   --Lists:
                   | show (dataTypeRep (dataTypeOf x)) == show (dataTypeRep (dataTypeOf [()])) = showChar '[' . foldr (\f g -> f . showComma . g) (showString "") (gmapQ (showsPrec' 0) x) . showChar ']'
                   --Binary tuples (show' for other tuples can be defined in a similar way):
                   | show (dataTypeRep (dataTypeOf x)) == show (dataTypeRep (dataTypeOf ((), ()))) = showChar '(' . drop 1 . foldr (\f g -> showChar ',' . f . g) (showString "") (gmapQ (showsPrec' 0) x) . showChar ')'
                   --Default infix case:
                   | isAlgType (dataTypeOf x) && constrFixity c == Infix = showParen (n > 0) $ ((inf1 . showString (showInfConstr c) . inf2))
                   --Default prefix case:
                   | otherwise = showParen (n > 10 && hasFields c) $ showString (showConstr c) . foldr (\f g -> showChar ' ' . f . g) (showString "") (gmapQ (showsPrec' 11) x)
        where c = toConstr x
              showParen b f = if b then showChar '(' . f . showChar ')' else f
              --hasFields assumes primitive types don't have fields
              hasFields = if isAlgType (dataTypeOf x) then not . null . constrFields else const False
              [inf1, inf2] = take 2 (gmapQ (showsPrec' 11) x)
              showInfConstr c = let s = showConstr c in take (length s - 2) (drop 1 s)
              showComma x = case take 2 x of
                               "[]" -> "]"
                               _   -> "," ++ drop 1 x
    
-- | Making Data instance of Read', we just modified the gread from the Data.Generics.Text library to handle cases where parentheses aren't necessary
instance Data a => Read' a where
    readsPrec' n = readP_to_S (gread')
             where
              -- Helper for recursive read
              gread' :: Data a' => ReadP a'
              gread' = baseCase True `extR` baseCase False
               where

                -- Determine result type
                myDataType = dataTypeOf (getArg (baseCase True))
                 where
                  getArg :: ReadP a'' -> a''
                  getArg = undefined
                  
                baseCase parens =
                  do
                     skipSpaces                     -- Discard leading space
                     _ <- if parens then char '(' else return ' '
                     skipSpaces                     -- Discard following space
                     str  <- parseConstr            -- Get a lexeme for the constructor
                     con  <- str2con str            -- Convert it to a Constr (may fail)
                     x    <- fromConstrM (readS_to_P (readsPrec' 11)) con -- Read the children
                     skipSpaces                     -- Discard leading space
                     _ <- if parens then char ')' else return ' '
                     skipSpaces                     -- Discard following space
                     if False then return x else (if not parens && n > 10 && isAlgType myDataType && (not . null . constrFields . toConstr) x then return (error "parantheses error") else return (error $ (show . constrFields . toConstr) x))
                     
                -- Turn string into constructor driven by the requested result type,
                -- failing in the monad if it isn't a constructor of this data type
                str2con :: String -> ReadP Constr
                str2con = maybe mzero return
                        . readConstr myDataType

                -- Get a Constr's string at the front of an input string
                parseConstr :: ReadP String
                parseConstr =
                           string "[]"     -- Compound lexeme "[]"
                      <++  string "()"     -- singleton "()"
                      <++  infixOp         -- Infix operator in parantheses
                      <++  readS_to_P lex  -- Ordinary constructors and literals

                -- Handle infix operators such as (:)
                infixOp :: ReadP String
                infixOp = do c1  <- char '('
                             str <- munch1 (not . (==) ')')
                             c2  <- char ')'
                             return $ [c1] ++ str ++ [c2]
        
--Some simple type to test with
data T a b = T a b deriving (Show, Typeable, Data)