{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DatatypeContexts #-}
module StateMonadPlus where

import Control.Monad.State

type StateMonadPlus s a = DiagnosticsT (StateT s (Either String)) a

data Diagnostics a 

data Monad m => DiagnosticsT m a

--instance MonadState StateMonadPlus where
    
    
class MonadState s m => StoreState s m | m -> s where
    saveState :: m ()
    loadState :: m ()
    
    
 
 
 