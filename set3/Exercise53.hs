{-#LANGUAGE FlexibleContexts#-}
import Control.Monad.Reader
import System.Random

one :: Int
one = 1

two :: Int
two = 2

randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n

sizedInt :: 
sizedInt = 
    do
        n <- ask
        g <- lift ask
        return (randomN n g)