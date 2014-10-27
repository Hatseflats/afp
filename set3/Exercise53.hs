{-#LANGUAGE FlexibleContexts, PolymorphicComponents#-}
module Exercise53 where

import Prelude hiding (Monad)
import qualified Control.Monad.Reader as R
import qualified System.Random as R

one :: Int
one = 1

two :: Int
two = 2

randomN :: (R.RandomGen g) => Int -> g -> Int
randomN n g = (fst (R.next g) `mod` (two * n + one)) - n

-- | sizedInt with the most general type possible
sizedInt :: (R.RandomGen g, R.MonadReader g m, R.MonadReader Int (t m), R.MonadTrans t) => t m Int
sizedInt = 
    do
        n <- R.ask
        g <- R.lift R.ask
        return (randomN n g)
        
--evidence translation--

data RandomGen g = RandomGen 
    {
        next :: g -> (Int, g)
    }

data Monad m = Monad 
    {
        bind :: forall a b. m a -> (a -> m b) -> m b,
        ret  :: forall a. a -> m a
    }

data MonadReader r m = MonadReader
    {
        monad :: Monad m,
        ask :: m r
    }
    
--MonadTrans quantifies over all monads, and therefore can't have a monad function like MonadReader
data MonadTrans t = MonadTrans
    {
        lift :: forall a m. Monad m -> m a -> t m a
    }

--Takes a record argument for RandomGen g, instead of using the class system
--The implementation is pretty straightforward..
randomN' :: RandomGen g -> Int -> g -> Int
randomN' randomRec n g = (fst (next randomRec g) `mod` (two * n + one)) - n
        
--Takes records for RandomGen, the 2 different MonadReader instances and MonadTrans, instead of using the class system
--We now have to make explicit which monad we want to use
sizedInt' :: RandomGen g -> MonadReader g m -> MonadReader Int (t m) -> MonadTrans t -> t m Int
sizedInt' randomRec readerRec1 readerRec2 transRec = 
    ask readerRec2                                    >>== (\n -> --Use the ask of readerRec2
    lift transRec (monad readerRec1) (ask readerRec1) >>== (\g -> --Use the ask of readerRec1, and lift it to the (t m) monad
    ret' (randomN' randomRec n g)))                               --Pass randomRec and the bound variables to randomN' and return the result
        where --We're working in the (t m) monad, which is defined by the readerRec2 record
            (>>==) = bind (monad readerRec2) 
            ret'   = ret  (monad readerRec2)