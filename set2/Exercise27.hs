{-#LANGUAGE FlexibleContexts#-}

module Exercise27 where

import Control.Monad.State 
import Control.Monad.Identity
import Control.Monad.Writer

type Object a = a -> a -> a

data X = X {n :: Int, f :: Int -> Int}

data Step a b = Enter a | Return b
    deriving Show

x, y, z :: Object X
x super this = X {n = 0, f = \i -> i + n this}
y super this = super {n = 1}
z super this = super {f = f super . f super}

zero :: Object a
zero super this = super 

extendedBy :: Object a -> Object a -> Object a
extendedBy o1 o2 super this = o2 (o1 super this) this

fixObject o = o (error "super") (fixObject o)

fac :: Monad m => Object (Int -> m Int)
fac super this n =
	case n of
		0 -> return 1
		n -> liftM (n*) (this (n - 1))

calls :: MonadState Int m => Object (a -> m b)
calls super this n = do
    modify (+1)
    super n

trace :: MonadWriter [Step a b] m => Object (a -> m b)
trace super this x = do
    tell [Enter x]
    r <- super x
    tell [Return r]
    return(r)

--main = do 
--	print (runWriter (fixObject (fac `extendedBy` trace) 3))


