import Control.Monad
import Data.List

data Counter = Counter {unCounter :: [(String, Int)]}

instance Show Counter where
    show (Counter xs) = "[" ++ intercalate ", " (map (\(s, n) -> "(" ++ s ++ "=" ++ show n ++ ")") xs) ++ "]"

add :: String -> Counter -> Counter
add s (Counter []) = Counter [(s, 1)]
add s (Counter ((s', n):xs)) | s == s'   = Counter ((s, n+1):xs)
                             | otherwise = Counter ((s', n):(unCounter (add s (Counter xs))))
                             
emptyCounter :: Counter
emptyCounter = Counter []

data StateMonadPlus s a = StateMonadPlus {runState :: (s, Counter) -> (a, s, Counter)}

instance Monad (StateMonadPlus s) where
    smp >>= f = StateMonadPlus $ \(s, c) -> let (x, s', c') = runState smp (s, c) in runState (f x) (s', add "bind" c')
    return x  = StateMonadPlus $ \(s, c) -> (x, s, add "return" c)
    
diagnostics :: StateMonadPlus s String
diagnostics = StateMonadPlus $ \(s, c) -> let c' = add "diagnostics" c in (show c', s, c')

test = (\(a, _, _) -> a) $ runState
 (do return 3 >> return 4
     return 5
     diagnostics)
    ((), emptyCounter)