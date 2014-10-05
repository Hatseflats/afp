module Exercise41 where

data F a = F {unF :: F a -> a}

y :: (a -> a) -> a
y = \f -> (\x -> f (unF x x)) (F (\x -> f (unF x x)))

--let's use it to define the factorial function
fac :: Int -> Int
fac = y f where
    f g 0 = 1
    f g n = n * g (n - 1)