{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module StateMonadPlus where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Error
import Data.List

class MonadState s m => StoreState s m | m -> s where
    saveState :: m ()
    loadState :: m ()
    
type StateMonadPlus s a = DiagnosticsT (StateHistoryT s (Either String)) a

--The counter, used to track function calls
data Counter = Counter [(String, Int)]

instance Show Counter where
    show (Counter xs) = "[" ++ intercalate ", " (map (\(ss, n) -> "(" ++ ss ++ "=" ++ show n ++ ")") xs) ++ "]"

--A monad transformer for maintaining a state and a history of saved states
data StateHistoryT s m a = StateHistoryT {runStateHistoryT :: s -> [s] -> m (a, s, [s])}

instance MonadTrans (StateHistoryT s) where
    lift x = StateHistoryT $ \s stack -> do
        x' <- x
        return (x', s, stack)
        
instance Monad m => Monad (StateHistoryT s m) where
    return x = StateHistoryT $ \s stack -> return (x, s, stack)
    x >>= f = StateHistoryT $ \s stack -> do
        (x', s', stack') <- runStateHistoryT x s stack
        runStateHistoryT (f x') s' stack'
        
instance Monad m => MonadState s (StateHistoryT s m) where
    get = StateHistoryT $ \s stack -> return (s, s, stack)
    put s = StateHistoryT $ \_ stack -> return ((), s, stack)

instance MonadError String m => StoreState s (StateHistoryT s m) where
    saveState = StateHistoryT $ \s stack -> return ((), s, (s:stack))
    loadState = StateHistoryT $ \s stack -> case stack of
        (s':stack') -> return ((), s', stack')
        _           -> throwError "y u tri load state if no saved??"
    
-- | The diagnostics monad maintains a counter which counts calls primitive functions
type Diagnostics a = DiagnosticsT Identity

data DiagnosticsT m a = DiagnosticsT {runDiagnosticsT :: Counter -> m (a, Counter)}

instance MonadTrans DiagnosticsT where
    lift x = DiagnosticsT $ \c -> do
        x' <- x
        return (x', c)

instance Monad m => Monad (DiagnosticsT m) where
    return x = DiagnosticsT $ \c -> return (x, increment "return" c)
    x >>= f = DiagnosticsT $ \c -> do
        (x', c') <- runDiagnosticsT x c
        runDiagnosticsT (f x') (increment "bind" c')

--Make monads with a diagnostics and a stateHistory component instance of MonadState (since StateMonadPlus we might as well generalize a bit)
instance Monad m => MonadState s (DiagnosticsT (StateHistoryT s m)) where
    get = DiagnosticsT $ \c -> do
              s <- get
              return (s, increment "get" c)
    put s = DiagnosticsT $ \c -> do
                put s
                return ((), increment "put" c)
                
--Make monads with a diagnostics and a stateHistory component instance of StoreState
instance MonadError String m => StoreState s (DiagnosticsT (StateHistoryT s m)) where
    saveState = DiagnosticsT $ \c -> do
        saveState
        return ((), increment "saveState" c)
    loadState = DiagnosticsT $ \c -> do
        loadState
        return ((), increment "loadState" c)

--A function that takes a function label of a called function and a counter, and updates the counter
increment :: String -> Counter -> Counter
increment s (Counter c) = Counter $ increment' s c where
    increment' s [] = [(s, 1)]
    increment' s ((s', n):xs) | s == s'   = (s, n+1):xs
                      | otherwise = (s', n):increment' s xs
                      
emptyCounter :: Counter
emptyCounter = Counter []

diagnostics :: StateMonadPlus s String
diagnostics = DiagnosticsT $ \c -> let c' = increment "diagnostics" c in return (show c', c')

annotate :: Monad m => String -> DiagnosticsT m a -> DiagnosticsT m a
annotate ss d = DiagnosticsT $ \c -> do (x, c') <- runDiagnosticsT d (increment ss c) --We increment with the label first, to maintain the order of the counter
                                        return (x, c')

runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus dT s = do ((x, c), s', _) <- runStateHistoryT (runDiagnosticsT dT emptyCounter) s []
                            return (x, s')

test = flip runStateMonadPlus undefined $ 
  do annotate "A" (return 3 >> return 4)
     annotate "A" $ return 5
     annotate "B" get
     put "state1"
     saveState
     put "state2"
     saveState
     saveState
     loadState
     loadState
     loadState
     diagnostics