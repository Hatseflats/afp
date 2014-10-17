{-#LANGUAGE FlexibleContexts, FlexibleInstances#-}

module Exercise42 where

type Square = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a = Nil
data Cons t a = Cons a (t a)

--The 2x2 identity matrix
id2 :: Square Int
id2 = Succ (Succ (Zero (Cons (Cons 1 (Cons 0 Nil)) (Cons (Cons 0 (Cons 1 Nil)) Nil))))

--The 3x3 matrix containing the numbers 1 to 9
oneToNine :: Square Int
oneToNine = Succ (Succ (Succ (Zero (Cons (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons (Cons 4 (Cons 5 (Cons 6 Nil))) (Cons (Cons 7 (Cons 8 (Cons 9 Nil))) Nil))))))


--Define show for kind *->* thingies. This is necessary because we can not quantify over a in the context Show (t a)
class ShowContainer t where
    showc :: Show' a => t a -> String
    
--Unfortunately, we need to use a special show class to avoid ambiguity with (for example) show instances of lists
class Show' a where
    show' :: a -> String
    
instance Show' Int where
    show' = show
    
instance Show' Char where
    show' = show
    
instance ShowContainer [] where
    showc = show . map show'
    
instance (ShowContainer t, Show' a) => Show' (t a) where
    show' = showc

instance (Show' a, Show' (t a)) => Show' (Cons t a) where
    show' (Cons x r) = ' ' : show' x ++ show' r
    
instance ShowContainer Nil where
    showc Nil = "\n"
    
instance ShowContainer t => ShowContainer (Cons t) where
    showc (Cons x r) = ' ' : show' x ++ showc r
    
instance ShowContainer t => ShowContainer (Square' t) where
    showc (Zero x) = showc x
    showc (Succ x) = showc x
    
--Finally, we can make Square instance of show
instance Show' a => Show (Square' Nil a) where
    show = show'