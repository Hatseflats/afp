{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module StateMonadPlus where

import Control.Monad.State
import Data.List

type StateMonadPlus s a = DiagnosticsT (StateT s (Either String)) a

--Counter
data Counter = Counter [(String, Int)]

instance Show Counter where
    show (Counter xs) = "[" ++ intercalate ", " (map (\(s, n) -> "(" ++ s ++ "=" ++ show n ++ ")") xs) ++ "]"

--The diagnostics monad
data Diagnostics a = Diagnostics {runDiagnostics :: Counter -> (a, Counter)}

instance Monad Diagnostics where
    d >>= f = Diagnostics $ \c -> let (x, c') = runDiagnostics d c in runDiagnostics (f x) (increment "bind" c')
    return x = Diagnostics $ \c -> (x, increment "return" c)
    
data DiagnosticsT m a = DiagnosticsT {runDiagnosticsT :: m (Diagnostics a)}

instance MonadTrans DiagnosticsT where
    lift x = DiagnosticsT $ do
        x' <- x
        return (Diagnostics $ \c -> (x', c))

instance Monad m => Monad (DiagnosticsT m) where
    return = lift . return
    d >>= f = DiagnosticsT $ do
        x <- runDiagnosticsT d
        runDiagnosticsT (f . fst . runDiagnostics x)
        
increment :: String -> Counter -> Counter
increment s (Counter c) = Counter $ increment' s c where
    increment' s [] = [(s, 1)]
    increment' s ((s', n):xs) | s == s'   = (s, n+1):xs
                      | otherwise = (s', n):increment' s xs
{-
instance MonadState s (StateMonadPlus s) where
    get = DiagnosticsT $ 
    put s = DiagnosticsT $
    
class MonadState s m => StoreState s m | m -> s where
    saveState :: m ()
    loadState :: m ()
    
    





                             
emptyCounter :: Counter
emptyCounter = Counter []


    
diagnostics :: StateMonadPlus s String
diagnostics = StateMonadPlus $ \s c -> let c' = increment "diagnostics" c in (show c', s, c')


test = (\(a, _, _) -> a) $ runStatePlus
 (do return 3 >> return 4
     return 5
     get
     put 23089893248983489349
     diagnostics)
    undefined emptyCounter -}