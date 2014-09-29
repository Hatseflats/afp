{-#LANGUAGE FlexibleInstances, MultiParamTypeClasses#-}
import Control.Monad.State
import Data.List

data Counter = Counter [(String, Int)]

instance Show Counter where
    show (Counter xs) = "[" ++ intercalate ", " (map (\(s, n) -> "(" ++ s ++ "=" ++ show n ++ ")") xs) ++ "]"

increment :: String -> Counter -> Counter
increment s (Counter c) = Counter $ increment' s c where
    increment' s [] = [(s, 1)]
    increment' s ((s', n):xs) | s == s'   = (s, n+1):xs
                      | otherwise = (s', n):increment' s xs
                             
emptyCounter :: Counter
emptyCounter = Counter []

data StateMonadPlus s a = StateMonadPlus {runStatePlus :: s -> Counter -> (a, s, Counter)}

instance Monad (StateMonadPlus s) where
    smp >>= f = StateMonadPlus $ \s c -> let (x, s', c') = runStatePlus smp s c in runStatePlus (f x) s' (increment "bind" c')
    return x  = StateMonadPlus $ \s c -> (x, s, increment "return" c)
    
diagnostics :: StateMonadPlus s String
diagnostics = StateMonadPlus $ \s c -> let c' = increment "diagnostics" c in (show c', s, c')

instance MonadState s (StateMonadPlus s) where
    get = StateMonadPlus  (\s c -> (s, s, increment "get" c))
    put s = StateMonadPlus (\_ c -> ((), s, increment "put" c))

test = (\(a, _, _) -> a) $ runStatePlus
 (do return 3 >> return 4
     return 5
     get
     put 23089893248983489349
     diagnostics)
    undefined emptyCounter