forceBoolList :: [Bool] -> [Bool]
forceBoolList b = b

forceBoolList' :: [Bool] -> r -> r 
forceBoolList' b n = undefined

main = print $ length (forceBoolList [True,undefined,2<3]) 
{-
force :: a -> a
force a = seq a a


-}