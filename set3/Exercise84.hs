forceBoolList :: [Bool] -> [Bool]
forceBoolList [] = []
forceBoolList (x:xs)
    | x == True = True:(forceBoolList xs)
    | x == False = False:(forceBoolList xs)

forceBoolList' :: [Bool] -> b -> b 
forceBoolList' [] r = r
forceBoolList' (x:xs) r
    | x == True = True:(forceBoolList' xs r)
    | x == False = False:(forceBoolList' xs r)

--main = print $ length ([True,2<3,undefined]) 
test = (1,seq undefined [1,2,3])

main = do
    print $ fst test
{-
force :: a -> a
force a = seq a a


-}